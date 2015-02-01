#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;

my $detailed_p = 0;
my $calorie_plot_file = '';
my $cho_pct_plot_file = '';
GetOptions('detailed!' => \$detailed_p,
	   'plot-cho-percent=s' => \$cho_pct_plot_file,
	   'plot-calories=s' => \$calorie_plot_file);
my $plot_p = $calorie_plot_file || $cho_pct_plot_file;

# Read the item/recipe database.
Food::Item->parse_recipes("recipes.text");

my @day_totals;

sub produce_day_total {
    my ($day_total) = @_;

    return
	unless $day_total;
    push(@day_totals, $day_total)
	if $plot_p;
    $day_total->present_summary(1, 1)
	if $detailed_p;
}

# Read the date files.
unshift(@ARGV, '-')
    unless @ARGV;
my @slots = qw(carbohydrate_grams fat_grams protein_grams calories);
for my $file (@ARGV) {
    my $meals = Food::Meal->parse_meals($file);
    my @totals;
    my $file_total = Food::Item->new(name => "$file total");
    my ($day_total, $current_day);
    for my $meal (@$meals) {
	if (($detailed_p || $plot_p)
	        && (! $day_total || $current_day ne $meal->date)) {
	    produce_day_total($day_total);
	    $day_total = Food::Item->new(name => $meal->date . ' total');
	    $current_day = $meal->date;
	}
	my @meal_totals = $meal->present_summary($detailed_p);
	for my $i (0 .. 3) {
	    next
		unless defined($meal_totals[$i]);
	    $totals[$i] += $meal_totals[$i];
	    my $slot = $slots[$i];
	    my $slot_value = $file_total->$slot() || 0;
	    $file_total->$slot($slot_value + $meal_totals[$i]);
	    next
		unless $day_total;
	    $slot_value = $day_total->$slot() || 0;
	    $day_total->$slot($slot_value + $meal_totals[$i]);
	}
    }
    produce_day_total($day_total);
    $file_total->present_summary(1, 1);
}

# Produce a calorie plot if requested.
if ($plot_p) {
    # Write the temp files.
    my $cho_pct_file = "$cho_pct_plot_file.tmp";
    my $cho_calorie_file = "$calorie_plot_file.cho.tmp";
    my $total_calorie_file = "$calorie_plot_file.total.tmp";
    {
	open(my $cho_pct_out, '>', $cho_pct_file)
	    or die;
	open(my $cho_calorie_out, '>', $cho_calorie_file)
	    or die;
	open(my $total_calorie_out, '>', $total_calorie_file)
	    or die;
	for my $day_total (@day_totals) {
	    my ($date) = split(' ', $day_total->name);
	    my $calories = $day_total->calories;
	    print $total_calorie_out ("$date\t$calories\n")
		if $calories;
	    my $cho_grams = $day_total->carbohydrate_grams;
	    if ($cho_grams) {
		my $cho_calories = 4 * $cho_grams;
		print $cho_calorie_out ("$date\t$cho_calories\n");
		print $cho_pct_out ("$date\t", 100 * $cho_calories / $calories,
				    "\n")
		    if $calories;
	    }
	}
    }

    # Generate the plot.
    if ($calorie_plot_file) {
	open(my $gnuplot, "| gnuplot > '$calorie_plot_file'")
	    or die "bug:  could not open gnuplot to '$calorie_plot_file':  $!";
	print $gnuplot ("set term png\n");
	print $gnuplot ("set boxwidth 0.8 relative\n");
	print $gnuplot ("set xdata time\n");
	print $gnuplot ("set timefmt '%d-%b-%y'\n");
	print $gnuplot ("plot '$total_calorie_file' using 1:2 with boxes ",
			"title 'Total calories', ",
			"'$cho_calorie_file' using 1:2 with boxes ",
			"title 'Carb calories';");
    }
    if ($cho_pct_plot_file) {
	open(my $gnuplot, "| gnuplot > '$cho_pct_plot_file'")
	    or die "bug:  could not open gnuplot to '$cho_pct_plot_file':  $!";
	print $gnuplot ("set term png\n");
	print $gnuplot ("set boxwidth 0.8 relative\n");
	print $gnuplot ("set xdata time\n");
	print $gnuplot ("set mytics 2\n");
	print $gnuplot ("set timefmt '%d-%b-%y'\n");
	print $gnuplot ("plot [] [0:60] '$cho_pct_file' using 1:2 with boxes ",
			"title 'Carb calories (percent)';");
    }
    # Clean up.
    unlink($cho_pct_file, $total_calorie_file, $cho_calorie_file);
}
exit(0);

###================
package Food::Base;

sub define_class_slots {
    # Called at BEGIN time to define slot accessor subs.
    my ($class, @slots) = @_;

    no strict 'refs';
    for my $accessor_method (@slots) {
	my $field = '_' . $accessor_method;
	*{$class.'::'.$accessor_method} = sub {
	    my $self = shift;
	    @_ ? ($self->{$field} = shift) : $self->{$field};
	};
    }
    1;
}

sub new {
    my $class = shift;

    my $self = bless({}, $class);
    while (@_) {
	my $method = shift;
	my $argument = shift;
	$self->$method($argument)
	    if $self->can($method);
    }
    return $self;
}

sub cleanup {
    my ($class, $string) = @_;

    $string =~ s/ *\([^()]*\)//g;
    $string =~ s/^ +//;
    return $string;
}

my (%weight_in_grams, %volume_in_ml);
BEGIN {
    %weight_in_grams
	= ('g' => 1,
	   'lb' => 453.5,
	   'kg' => 1000,
	   'oz' => 28.3);
    %volume_in_ml
	= ('ml' => 1,
	   'l' => 1000,
	   'L' => 1000,
	   'T' => 15,
	   'g' => 1,	# cheat.
	   'tsp' => 5);
}

sub convert {
    my ($self, $from_amount, $from_units, $to_amount, $to_units, $what) = @_;
    ($to_amount, $to_units) = ($1, $2)
	if ! $to_units && $to_amount =~ /^([\d.]+)(\S+)$/;

    my $from_factor = $weight_in_grams{$from_units};
    if ($from_factor) {
	my $to_factor = $weight_in_grams{$to_units};
	return $from_amount * $from_factor / ($to_amount * $to_factor)
	    if $to_factor;
    }
    elsif ($from_factor = $volume_in_ml{$from_units}) {
	my $to_factor = $volume_in_ml{$to_units};
	return $from_amount * $from_factor / ($to_amount * $to_factor)
	    if $to_factor;
    }
    warn "oops, can't convert '$from_units' to '$to_units', for $what";
    return $from_amount;
}

sub show_total {
    # This just returns the string.
    my ($class, $total, $missing_p) = @_;

    if (! defined($total)) {
	"\t     ?";
    }
    elsif (! $missing_p) {
	sprintf "\t%6.1f", $total;
    }
    else {
	sprintf "\t%6.1f+", $total;
    }
}

###================
package Food::Item;

# A food item has a name, a serving size, and specific nutritional values per
# serving.  It can be a pure food item (e.g. an apple), a processed item with
# its nutritional values from the package, or (as a Food::Recipe) a collection
# of other food items with the sum of their nutritional values.  A Food::Item
# is the categorical object of which a Food::Serving is an instance for a
# particular meal.

use base qw(Food::Base);

BEGIN {
    Food::Item->define_class_slots
	(qw(name serving_size protein_grams fat_grams carbohydrate_grams
	    fiber_grams calories cholesterol_mg sodium_mg));
}

our %item_from_name;

my %message_and_units_from_name;
BEGIN {
    %message_and_units_from_name
	= (protein => [ qw(protein_grams g) ],
	   fat => [ qw(fat_grams g) ],
	   carbohydrate => [ qw(carbohydrate_grams g) ],
	   calories => [ 'calories', '' ],
	   fiber => [ qw(fiber_grams g) ],
	   cholesterol => [ qw(cholesterol_mg mg) ],
	   sodium => [ qw(sodium_mg mg) ]);
}

sub add_item {
    my ($self, $item, $amount, $units) = @_;

    warn($self->name, " is an item, not a recipe; can't add ",
	 $item->name, " to it.\n");
}

sub carbohydrate_percent {
    my ($self) = @_;

    my $cho_grams = $self->carbohydrate_grams;
    my $calories = $self->calories;
    return
	unless defined($cho_grams) && defined($calories);
    return 100.0 * (4 * $cho_grams) / $calories;
}

sub present_summary {
    my ($self, $detailed_p, $n_servings) = @_;
    $n_servings = 1
	unless defined($n_servings);

    printf('%-32s', '  ' . $self->name);
    for my $slot (qw(carbohydrate_grams fat_grams
		     protein_grams calories)) {
	my $value = $self->$slot();
	$value *= $n_servings
	    if defined($value);
	print $self->show_total($value, ! defined($value));
    }
    printf "\t%3.2fsvg", $n_servings
	if $n_servings != 1;
    if ($detailed_p) {
	my $cho_percent = $self->carbohydrate_percent;
	printf(" CHO%%%.1f", $cho_percent)
	    if defined($cho_percent);
    }
    print "\n";
}

sub parse_recipes {
    my ($class, $file_name) = @_;

    open(my $stream, '<', $file_name)
	or die "bug";
    my $current_item;
    while (<$stream>) {
	chomp;
	s/^\s+//;
	next
	    # Skip comments and blank lines.
	    if ! $_ || /^#/;
	if (/^\[(\S+)\s+(.*)\]$/) {
	    # Heading line.
	    my ($type, $name) = //;
	    $current_item->finalize()
		if $current_item && $current_item->can('finalize');
	    if ($type eq 'item') {
		$current_item = Food::Item->new(name => $name);
		$item_from_name{$name} = $current_item;
	    }
	    elsif ($type eq 'recipe') {
		$current_item = Food::Recipe->new(name => $name);
		$item_from_name{$name} = $current_item;
	    }
	    else {
		warn "$file_name:$.:  Unknown type '$type'.\n";
		undef($current_item);
	    }
	}
	elsif (! $current_item) {
	    # Just ignore orphaned data.
	}
	elsif (/^serving size:\s*(.*)/) {
	    $current_item->serving_size($1);
	}
	elsif (/^servings:\s*(.*)/) {
	    $current_item->n_servings($1);
	}
	elsif (/^alias:\s*(.*)/) {
	    # Aliases are cheap.
	    $item_from_name{$1} = $current_item;
	}
	elsif (/^([\d.]+)(\S*)\s+(.*)/) {
	    # Data value.
	    my ($amount, $units, $name) = //;
	    my $mu = $message_and_units_from_name{$name};
	    if ($mu) {
		my ($message, $desired_units) = @$mu;
		if (! $desired_units || $units eq $desired_units) {
		    $current_item->$message($amount);
		}
		else {
		    die "$0:  Can't convert from $units to $desired_units.\n";
		}
	    }
	    elsif (my $item = $item_from_name{$class->cleanup($name)}) {
		$current_item->add_item($item, $amount, $units);
	    }
	    else {
		warn "$file_name:$.:  Don't know what to do with '$name'.\n";
	    }
	}
	elsif (/^(source|nominal size):/) {
	    # Ignore these for now.
	}
	elsif (my $item = $item_from_name{$class->cleanup($_)}) {
	    $current_item->add_item($item, 1, 'serving');
	}
	else {
	    warn "$file_name:$.:  Don't know what to do with '$_'.\n";
	}
    }
    $current_item->finalize()
	if $current_item;
}

###================
package Food::Ingredient;

# An ingredient is a particular food item eaten during a particular meal, or
# added to a particular recipe, both with a specified serving amount, which is
# always a multiple of the item's serving size.

use base qw(Food::Base);

BEGIN {
    Food::Ingredient->define_class_slots(qw(item amount units n_servings));
}

sub new {
    my ($class, @args) = @_;

    my $self = $class->SUPER::new(@args);

    # Figure out our serving size.
    my $verbose_p = 0; # $item->name =~ /cook/;
    my $amount = $self->amount || 0;
    if (! $amount) {
	$self->serving_size(0);
	return $self;
    }
    my $units = $self->units || '';
    my $item = $self->item or die;
    my $item_serving_size = $item->serving_size;
    if (! defined($item_serving_size)
	    || $units eq '' || $units eq 'serving' || $units eq 'svg') {
	$self->n_servings($amount);
    }
    else {
	my $n_servings;
	my ($item_qty, $item_units)
	    = ($item_serving_size =~ /^([\d.]+)(\S+)$/ ? ($1, $2)
	       : $item_serving_size =~ /^(\d+) / ? ($1, 'each')
	       : (1, 'each'));
	if (lc($units) ne lc($item_units)) {
	    $n_servings = $self->convert($amount, $units,
					 $item_qty, $item_units, $item->name);
	    warn("converted $amount '$units' to '$item_serving_size' to get ",
		 " $n_servings servings for ", $item->name, ".\n")
		if 0;
	}
	else {
	    $n_servings = $amount / $item_qty;
	}
	$self->n_servings($n_servings);
    }
    return $self;
}

###================
package Food::Recipe;

# A recipe is a food item that is itself made from one or more ingredients,
# which refer to other recipes or food items.  The food values are stated per
# serving, and are computed once from the ingredient food values.

use base qw(Food::Item);

my $slot_pairs;

BEGIN {
    Food::Recipe->define_class_slots
	(qw(ingredients n_servings),
	 qw(protein_complete_p fat_complete_p carbohydrate_complete_p
	    calories_complete_p cholesterol_complete_p sodium_complete_p));
    $slot_pairs
	= [ [ qw(carbohydrate_complete_p carbohydrate_grams) ],
	    [ qw(fat_complete_p fat_grams) ],
	    [ qw(protein_complete_p protein_grams) ],
	    [ qw(calories_complete_p calories) ],
	    [ qw(cholesterol_complete_p cholesterol_mg) ],
	    [ qw(sodium_complete_p sodium_mg) ] ];
}

sub add_item {
    my ($self, $item, $amount, $units) = @_;

    my $ingredient = Food::Ingredient->new(item => $item,
					   amount => $amount,
					   units => $units);
    push(@{$self->{_ingredients}}, $ingredient);
    return $ingredient;
}

sub finalize {
    # Assuming we have all of our ingredients, compute our total food values.
    my ($self, $verbose_p) = @_;

    my $ingredients = $self->ingredients;
    return $self
	unless $ingredients;

    my $n_servings = $self->n_servings;
    if (! $n_servings) {
	# [hack: try to compute total weight.  -- rgr, 1-Jan-15.]
	my $total_weight = 0;
	for my $ingredient (@$ingredients) {
	    my $item = $ingredient->item;
	}
    }
    if (! $n_servings) {
	warn($self->name,
	     ":  Don't know how many servings we are; assuming 6.\n");
	$n_servings = 6;
    }

    # Compute other values.
    for my $slot_pair (@$slot_pairs) {
	my ($complete_slot, $value_slot) = @$slot_pair;
	my ($total, $missing_p);
	for my $ingredient (@$ingredients) {
	    my $item = $ingredient->item;
	    my $value = $item->$value_slot();
	    if (defined($value)) {
		$total += $ingredient->n_servings * $value;
		warn($item->name, " $value_slot value $value, servings ",
		     $ingredient->n_servings, ", total $total\n")
		    if $verbose_p;
	    }
	    else {
		warn($self->name, ":  ", $item->name,
		     " $value_slot is missing");
		$missing_p++;
	    }
	}
	$self->$value_slot(defined($total) ? $total/$n_servings : $total);
	$self->$complete_slot(! $missing_p);
    }
    return $self;
}

###================
package Food::Meal;

# A meal is a collection of ingredients eaten together (loosely) on a
# particular date.

use base qw(Food::Base);

BEGIN {
    Food::Meal->define_class_slots
	(qw(date meal ingredients indeterminate_p));
}

sub add_item {
    my ($self, $item, $amount, $units) = @_;

    warn "adding ", $item->name, " in amount '$amount', units '$units'\n"
	if 0 && $item->name =~ /cauliflower dish/;
    my $ingredient = Food::Ingredient->new(item => $item,
					   amount => $amount,
					   units => $units);
    push(@{$self->{_ingredients}}, $ingredient);
    return $ingredient;
}

sub parse_meals {
    my ($class, $file_name) = @_;

    open(my $stream, '<', $file_name)
	or die "$0:  Can't open '$file_name' for input:  $!";
    my ($current_date, $current_meal_name, $current_meal);
    my $meals = [ ];
    my %meal_from_day_and_name;
    my $register_meal = sub {
	my $meal = $meal_from_day_and_name{$current_date}{$current_meal_name};
	return $meal
	    if $meal;
	$meal = $class->new(date => $current_date,
			    meal => $current_meal_name);
	$meal_from_day_and_name{$current_date}{$current_meal_name} = $meal;
	push(@$meals, $meal);
	return $meal;
    };

    while (<$stream>) {
	chomp;
	s/^\s+//;
	next
	    # Skip comments and blank lines.
	    if ! $_ || /^#/;
	if (/^(\d+-\w+-\d+):/) {
	    $current_date = $1;
	    $current_meal_name = 'breakfast';
	}
	elsif (/(breakfast|lunch|snack|dinner):$/i) {
	    $current_meal_name = lc($1);
	}
	elsif (/^([\d.]+)(\S*)\s+(.*)/) {
	    my ($amount, $units, $name) = //;
	    $units = 'serving'
		if $name =~ s/^(serving|svg)s?\s+//;
	    my $item = $item_from_name{Food::Item->cleanup($name)};
	    if (! $item) {
		warn "Can't find '$name' to add it to the current meal.\n";
		# Make a fake item to record our uncertainty.
		$item = Food::Item->new(name => $name);
	    }
	    $current_meal = $register_meal->();
	    $current_meal->add_item($item, $amount, $units);
	}
	elsif (my $item = $item_from_name{$class->cleanup($_)}) {
	    $current_meal = $register_meal->();
	    $current_meal->add_item($item, 1, 'serving');
	}
	elsif (/random/i) {
	    # Don't bother with these.
	}
	else {
	    warn "Can't find '$_' to add it to the current meal.\n";
	    # Make a fake item to record our uncertainty.
	    $item = Food::Item->new(name => $_);
	    $current_meal = $register_meal->();
	    $current_meal->add_item($item, 1, 'serving');
	}
    }
    return $meals;
}

sub present_summary {
    # [ . . . though if it's detailed, it's not a summary.  -- rgr, 28-Dec-14.]
    my ($self, $detailed_p) = @_;

    my $verbose_p = 0; # $self->meal eq 'snack';
    my $ingredients = $self->ingredients || [ ];
    for my $ingredient (@$ingredients) {
	my $item = $ingredient->item;
    }

    my @totals;
    printf('%-32s', $self->date . ' ' . $self->meal . ':');
    my ($cho_grams, $calories);
    for my $slot (qw(carbohydrate_grams fat_grams protein_grams calories)) {
	my ($total, $missing_p);
	for my $ingredient (@$ingredients) {
	    my $item = $ingredient->item;
	    my $value = $item->$slot();
	    if (defined($value)) {
		warn($item->name, " $slot value $value, servings ",
		     $ingredient->n_servings)
		    if $verbose_p;
		$total += $ingredient->n_servings * $value;
	    }
	    else {
		# warn $item->name, " $slot is missing";
		$missing_p++;
	    }
	}
	if ($slot eq 'carbohydrate_grams') {
	    $cho_grams = $total;
	}
	elsif ($slot eq 'calories') {
	    $calories = $total;
	}
	push(@totals, $total);
	print $self->show_total($total, $missing_p);
    }
    printf(" CHO%%%.1f", 100.0 * (4 * $cho_grams) / $calories)
	if defined($cho_grams) && defined($calories);
    print("\n");

    if ($detailed_p) {
	for my $ingredient (@$ingredients) {
	    my $item = $ingredient->item;
	    my $n_servings = $ingredient->n_servings;
	    $ingredient->item->present_summary(0, $ingredient->n_servings);
	}
    }
    return @totals;
}
