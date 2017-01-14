#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use IO::File;

my $detailed_p = 0;
my $daily_p = 0;
my $tail_length = 0;
my $recipe_file_name = 'recipes.text';
my $calorie_plot_file = '';
my $cho_pct_plot_file = '';
my (@show_items, @recipes_matching, @ingredients_matching);
GetOptions('detailed!' => \$detailed_p,
	   'daily!' => \$daily_p,
	   'recipe-file=s' => \$recipe_file_name,
	   'plot-cho-percent=s' => \$cho_pct_plot_file,
	   'show-item=s' => \@show_items,
	   'tail=i' => \$tail_length,
	   'recipes-matching=s' => \@recipes_matching,
	   'ingredients-matching=s' => \@ingredients_matching,
	   'plot-calories=s' => \$calorie_plot_file);
my $plot_p = $calorie_plot_file || $cho_pct_plot_file;

# Read the item/recipe database.
Food::Item->parse_recipes($recipe_file_name);

# Look for matching recipes.
if (@recipes_matching || @ingredients_matching) {
    Food::Item->show_matching_recipes(\@recipes_matching,
				      \@ingredients_matching);
    exit(0)
	unless @show_items || @ARGV;
}

# Do items we've been asked to show.
for my $item_name (@show_items) {
    my $item = Food::Item->fetch_item($item_name);
    if (! $item) {
	warn "$0:  Can't find '$item_name' in our database.\n";
	next;
    }

    $item->present_summary(display_cho_p => 1);
    my $total_weight = 0;
    my $ingredients = $item->ingredients;
    next
	unless $ingredients;
    for my $ing (sort { $b->n_servings * $b->item->net_carbohydrate_grams
			    <=> ($a->n_servings
				 * $a->item->net_carbohydrate_grams);
		 } @$ingredients) {
	my $it = $ing->item;
	my $n_svg = $ing->n_servings;
	my $calories = $n_svg * $it->calories;
	my $carbs = $n_svg * $it->net_carbohydrate_grams;
	my $fat = $n_svg * $it->fat_grams;
	my $protein = $n_svg * $it->protein_grams;
	my $cho_pct = $calories ? 100.0 * (4 * $carbs) / $calories : 0;
	my $units = $ing->units || '';
	$units = ''
	    if $units eq 'serving';
	printf("    %-28s  %s %s %s %s CHO%%%.1f\n",
	       ($ing->amount || '') . $units . ' ' . $it->name,
	       $item->show_total($carbs), $item->show_total($fat),
	       $item->show_total($protein),
	       $item->show_total($calories), $cho_pct);
    }
}

my @day_totals;

sub produce_day_total {
    my ($day_total) = @_;

    return
	unless $day_total;
    push(@day_totals, $day_total)
	if $plot_p;
    $day_total->present_summary(detailed_p => $detailed_p,
				display_cho_p => 1)
	if $detailed_p || $daily_p;
}

# Read the meal files.
unshift(@ARGV, '-')
    unless @ARGV || @show_items;
my @slots = qw(net_carbohydrate_grams fat_grams protein_grams calories);
for my $file (@ARGV) {
    # Find meals, and maybe ellipsize.
    my $meals = Food::Meal->parse_meals($file);
    if ($tail_length) {
	# $tail_length is in terms of days.
	my $tail_meals = [ ];
	my $current_date = '';
	my $day_count = 0;
	for my $meal (reverse(@$meals)) {
	    my $date = $meal->date;
	    if ($date ne $current_date) {
		$day_count++;
		last
		    if $day_count > $tail_length;
		$current_date = $date;
	    }
	    unshift(@$tail_meals, $meal);
	}
	$meals = $tail_meals;
    }

    # Generate output.
    my @totals;
    my $file_total = Food::Item->new(name => "$file total:");
    my ($day_total, $current_day);
    for my $meal (@$meals) {
	if (($detailed_p || $daily_p || $plot_p)
	        && (! $day_total || $current_day ne $meal->date)) {
	    produce_day_total($day_total);
	    $day_total = Food::Item->new(name => $meal->date . ' total:');
	    $current_day = $meal->date;
	}
	$meal->present_summary(detailed_p => $detailed_p,
			       colon_p => 1,
			       display_cho_p => 1);
	my $i = 0;
	for my $slot (@slots) {
	    my $value = $meal->$slot();
	    next
		unless defined($value);
	    $totals[$i++] += $value;
	    my $slot_value = $file_total->$slot() || 0;
	    $file_total->$slot($slot_value + $value);
	    next
		unless $day_total;
	    $slot_value = $day_total->$slot() || 0;
	    $day_total->$slot($slot_value + $value);
	}
    }
    produce_day_total($day_total);
    $file_total->present_summary(detailed_p => $detailed_p,
				 display_cho_p => 1)
	unless $daily_p;
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
	    my $cho_grams = $day_total->net_carbohydrate_grams;
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
	print $gnuplot ("plot [] [0:3500] ",
			"'$total_calorie_file' using 1:2 with boxes ",
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
    return lc($string);
}

my (%weight_in_grams, %volume_in_ml);
BEGIN {
    %weight_in_grams
	= ('g' => 1,
	   'mg' => 1e-3,
	   'lb' => 453.5,
	   'kg' => 1000,
	   'oz' => 28.3);
    %volume_in_ml
	= ('ml' => 1,
	   'l' => 1000,
	   'L' => 1000,
	   'T' => 15,
	   'C' => 236.6,
	   'floz' => 29.57,
	   'tsp' => 5);
}

sub weight_units_p {
    my ($class, $units) = @_;

    return $weight_in_grams{$units};
}

sub volume_units_p {
    my ($class, $units) = @_;

    return $volume_in_ml{$units};
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

sub parse_units {
    # Returns ($amount, $units, $unit_class, $name) if $string starts with a
    # number and optional units, where $unit_class is "serving", "weight", or
    # "volume", else nothing.
    my ($class, $string, $verbose_p) = @_;

    # Extract the amount.
    my ($amount, $unit_string);
    if ($string =~ m@^\s*(\d+)/(\d+)\s*(.*)@) {
	# Fraction.
	($amount, $unit_string) = ($1 / $2, $3);
    }
    elsif ($string =~ m@^\s*(\d+)-(\d+)/(\d+)\s*(.*)@) {
	# Integer with fraction.
	($amount, $unit_string) = ($1 + $2/$3, $4);
    }
    elsif ($string =~ m@^\s*(\.\d+|\d+\.\d*)\s*(.*)@) {
	# Integer or decimal fraction.
	($amount, $unit_string) = ($1, $2);
    }
    elsif ($string =~ m@^\s*(\d+)\s*(.*)@) {
	# Integer.
	($amount, $unit_string) = ($1, $2);
    }
    else {
	return;
    }

    # Now look for units.
    if ($unit_string =~ m@^\s*([a-zA-Z]+)\s+(.+)@) {
	my ($units, $name) = ($1, $2);
	my $unit_class;
	if ($units =~ /(svg|serving)s?$/i) {
	    $unit_class = $units = 'serving';
	}
	elsif ($weight_in_grams{$units}) {
	    $unit_class = 'weight';
	}
	elsif ($volume_in_ml{$units}) {
	    $unit_class = 'volume';
	}
	else {
	    # Assume "serving" was meant, so $units is part of the name.
	    $name = $unit_string;
	    $unit_class = $units = 'serving';
	}
	warn "have '$string' => ($amount, $units, $name)\n"
	    if $verbose_p;
	return ($amount, $units, $unit_class, $name);
    }
    else {
	# An amount, but no units, so this must be the number of servings.
	warn "have '$string' => ($amount, 'serving', $unit_string)\n"
	    if $verbose_p;
	return ($amount, 'serving', 'serving', $unit_string);
    }
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

# A food item has a name, a serving size (in terms of volume,
# weight, or both), and specific nutritional values per
# serving.  It can be a pure food item (e.g. an apple), a processed item with
# its nutritional values from the package, or (as a Food::Recipe) a collection
# of other food items with the sum of their nutritional values.

use base qw(Food::Base);

BEGIN {
    Food::Item->define_class_slots
	(qw(name serving_size_g serving_size_ml
	    protein_grams fat_grams carbohydrate_grams
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

sub net_carbohydrate_grams {
    my $self = shift;

    if (@_) {
	$self->carbohydrate_grams(shift);
	$self->fiber_grams(0);
    }
    else {
	my $cho_grams = $self->carbohydrate_grams;
	return
	    unless defined($cho_grams);
	return $cho_grams - ($self->fiber_grams || 0);
    }
}

sub fetch_item {
    my ($class, $item_name) = @_;

    my $clean_name = $class->cleanup($item_name);
    my $item = $item_from_name{$clean_name};
    return $item
	if $item;
    return $item_from_name{$clean_name}
        if $clean_name =~ s/s$//;
}

sub add_ingredient {
    my ($self, $item, $amount, $units) = @_;

    warn($self->name, " is an item, not a recipe; can't add ",
	 $item->name, " to it.\n");
}

sub ingredients { }

sub carbohydrate_percent {
    my ($self) = @_;

    my $cho_grams = $self->net_carbohydrate_grams;
    my $calories = $self->calories;
    return
	unless defined($cho_grams) && defined($calories);
    return 100.0 * (4 * $cho_grams) / $calories;
}

sub present_summary {
    my ($self, %keys) = @_;
    my $n_servings = $keys{n_servings} || 1;

    my $name = $self->name;
    $name .= ':'
	if $keys{colon_p};
    $name = "  $name"
	if $keys{indent_name_p};
    printf('%-32s', $name);
    for my $slot (qw(net_carbohydrate_grams fat_grams
		     protein_grams calories)) {
	my $value = $self->$slot();
	$value *= $n_servings
	    if defined($value);
	print $self->show_total($value, ! defined($value));
    }
    if ($keys{display_cho_p}) {
	my $cho_percent = $self->carbohydrate_percent;
	printf(" CHO%%%.1f", $cho_percent)
	    if defined($cho_percent);
    }
    elsif ($n_servings != 1) {
	printf "\t%3.2fsvg", $n_servings;
    }
    print "\n";
    if ($keys{detailed_p} && $self->can('ingredients') && $self->ingredients) {
	for my $ingredient (@{$self->ingredients}) {
	    my $item = $ingredient->item;
	    my $n_servings = $ingredient->n_servings;
	    $ingredient->item->present_summary
		(n_servings => $ingredient->n_servings,
		 indent_name_p => 1);
	}
    }
}

sub parse_recipes {
    my ($class, $file_name) = @_;

    my $open_file = sub {
	my ($file_name, $base_file_name) = @_;

	my $stream = IO::File->new($file_name, '<');
	if (! $stream && $base_file_name && $base_file_name =~ m@(.+)/@) {
	    # Try something relative to $base_file_name.
	    my $local_file_name = "$1/$file_name";
	    $stream = IO::File->new($local_file_name, '<');
	}
	return $stream;
    };

    my @include_stack;		# [$stream, $name] for where included from.
    my $warning = sub {
	# Emit a warning prefixed by the @include_stack contents.

	for my $entry (@include_stack) {
	    my ($stream, $name) = @$entry;
	    warn "$0:  From $name line ", $stream->input_line_number, ":\n";
	}
	warn("$0:  ", @_);
    };

    my $store_item = sub {
	# Put the item in the %item_from_name hash under $name, warning about
	# any conflicts.
	my ($name, $item) = @_;

	my $lc_name = lc($name);
	my $existing_item = $item_from_name{$lc_name};
	if ($existing_item) {
	    $warning->("Conflict for '$name'.\n");
	}
	else {
	    $item_from_name{$lc_name} = $item;
	}
    };

    my $parse_file;
    $parse_file = sub {
	my ($file_name, $base_file_name) = @_;

	my $stream = $open_file->($file_name, $base_file_name);
	if (! $stream) {
	    $warning->("Can't open '$file_name':  $!");
	    return;
	}
	push(@include_stack, [ $stream, $file_name ]);

	my $current_item;
	while (<$stream>) {
	    chomp;
	    s/^\s+//;
	    if (! $_ || /^#/) {
		# Skip comments and blank lines.
	    }
	    elsif (/^include (.*)/) {
		my $include_file_name = $1;
		# warn "include_file_name $include_file_name";
		$current_item->finalize()
		    if $current_item && $current_item->can('finalize');
		$parse_file->($include_file_name, $file_name);
	    }
	    elsif (/^\[(\S+)\s+(.*)\]$/) {
		# Heading line.
		my ($type, $name) = //;
		$current_item->finalize()
		    if $current_item && $current_item->can('finalize');
		if ($type eq 'item') {
		    $current_item = Food::Item->new(name => $name);
		    $store_item->($name, $current_item);
		}
		elsif ($type eq 'recipe') {
		    $current_item = Food::Recipe->new(name => $name);
		    $store_item->($name, $current_item);
		}
		else {
		    $warning->("Unknown type '$type'.\n");
		    undef($current_item);
		}
	    }
	    elsif (! $current_item) {
		# Just ignore orphaned data.
	    }
	    elsif (/^serving size:\s*(.*)/) {
		my $size = $1;
		my ($amount, $units, $unit_class, $name)
		    = $class->parse_units("$size foo");
		if ($unit_class eq 'weight') {
		    my $weight = $class->convert($amount, $units, 1, 'g');
		    $current_item->serving_size_g($weight);
		}
		elsif ($unit_class eq 'volume') {
		    my $volume = $class->convert($amount, $units, 1, 'ml');
		    $current_item->serving_size_ml($volume);
		}
		else {
		    $warning->("Can't parse serving size '$size', for ",
			       $current_item->name, "\n.");
		}
	    }
	    elsif (/^servings:\s*(.*)/) {
		$current_item->n_servings($1);
	    }
	    elsif (/^alias:\s*(.*)/) {
		# Aliases are cheap.
		$store_item->($1, $current_item);
	    }
	    elsif ($class->parse_units($_)) {
		# Data value.
		my ($amount, $units, $unit_class, $name)
		    = $class->parse_units($_);
		my $mu = $message_and_units_from_name{$name};
		if ($mu) {
		    my ($message, $desired_units) = @$mu;
		    if (! $desired_units || $units eq $desired_units) {
			$current_item->$message($amount);
		    }
		    else {
			# [we could do better here.  -- rgr, 4-Apr-15.]
			die("$0:  Can't convert from $units ",
			    "to $desired_units.\n");
		    }
		}
		elsif (my $item = $class->fetch_item($name)) {
		    $current_item->add_ingredient($item, $amount, $units);
		}
		else {
		    $warning->("Don't know what to do with '$name'.\n");
		}
	    }
	    elsif (/^(source|nominal size):/) {
		# Ignore these for now.
	    }
	    elsif (my $item = $class->fetch_item($_)) {
		$current_item->add_ingredient($item, 1, 'serving');
	    }
	    else {
		$warning->("Don't know what to do with '$_'.\n");
	    }
	}
	$current_item->finalize()
	    if $current_item && $current_item->can('finalize');
	pop(@include_stack);
    };

    # Main code.
    $parse_file->($file_name);
}

sub show_matching_recipes {
    my ($class, $name_regexps, $ingredient_regexps) = @_;

    my @recipes = sort { $a->name cmp $b->name;
    } grep {
	if ($_->isa('Food::Recipe')) {
	    my $name = $_->name;
	    my $result = 0;
	    for my $re (@$name_regexps) {
		$result = 1, last
		    if $name =~ /$re/i;
	    }
	    my $ingredients = $_->ingredients;
	    if (! $result && $ingredients
		&& @$ingredients && @$ingredient_regexps) {
		for my $ingredient (@$ingredients) {
		    my $name = $ingredient->item->name;
		    for my $re (@$ingredient_regexps) {
			$result = 1, last
			    if $name =~ /$re/i;
		    }
		}
	    }
	    $result;
	}
    } values(%item_from_name);
    for my $recipe (@recipes) {
	$recipe->present_summary();
    }
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
	$self->n_servings(0);
	return $self;
    }
    my $units = $self->units || '';
    my $item = $self->item or die;
    if ($units eq 'serving') {
	$self->n_servings($amount);
    }
    else {
	my ($n_servings, $from_factor);
	# [units are case-sensitive elsewhere.  -- rgr, 4-Apr-15.]
	if ($item->serving_size_ml
	       && ($from_factor = $self->volume_units_p($units))) {
	    $n_servings = $amount * $from_factor / $item->serving_size_ml;
	}
	elsif ($item->serving_size_g
	       && ($from_factor = $self->weight_units_p($units))) {
	    $n_servings = $amount * $from_factor / $item->serving_size_g;
	}
	else {
	    warn("$0:  No conversion from $amount$units to ",
		 "item units for ", $item->name, ".\n")
		if $item->serving_size_g || $item->serving_size_ml;
	    # punt.
	    $n_servings = $amount;
	}
	$self->n_servings($n_servings);
    }
    return $self;
}

###================
package Food::HasIngredients;

# Base class for recipes and meals, both of which have ingredients.

use base qw(Food::Item);

my $slot_pairs;
BEGIN {
    Food::HasIngredients->define_class_slots
	(qw(ingredients),
	 qw(protein_complete_p fat_complete_p carbohydrate_complete_p
	    calories_complete_p cholesterol_complete_p sodium_complete_p));
    $slot_pairs
	= [ [ qw(carbohydrate_complete_p net_carbohydrate_grams) ],
	    [ qw(fat_complete_p fat_grams) ],
	    [ qw(protein_complete_p protein_grams) ],
	    [ qw(calories_complete_p calories) ],
	    [ qw(cholesterol_complete_p cholesterol_mg) ],
	    [ qw(sodium_complete_p sodium_mg) ] ];
}

sub n_ingredients {
    my ($self) = @_;

    my $ingredients = $self->ingredients;
    return $ingredients ? scalar(@$ingredients) : 0;
}

sub add_ingredient {
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
		     " $value_slot is missing")
		    if $verbose_p;
		$missing_p++;
	    }
	}
	$self->$value_slot(defined($total) ? $total/$n_servings : $total);
	$self->$complete_slot(! $missing_p);
    }
    return $self;
}

###================
package Food::Recipe;

# A recipe is a food item that is itself made from one or more ingredients,
# which refer to other recipes or food items.  The food values are stated per
# serving, and are computed once from the ingredient food values.

use base qw(Food::HasIngredients);

BEGIN {
    Food::Recipe->define_class_slots(qw(n_servings));
}

###================
package Food::Meal;

# A meal is a collection of ingredients eaten together (loosely) on a
# particular date.

use base qw(Food::HasIngredients);

BEGIN {
    Food::Meal->define_class_slots(qw(date meal));
}

sub n_servings { 1; }

sub name {
    my ($self) = @_;

    return join(' ', $self->date, $self->meal);
}

sub parse_meals {
    my ($class, $file_name) = @_;

    open(my $stream, "<$file_name")
	or die "$0:  Can't open '$file_name' for input:  $!";
    my ($current_date, $current_meal_name, $current_meal);
    my $meals = [ ];
    my $register_meal = sub {
	if ($current_meal && $current_meal->n_ingredients) {
	    $current_meal->finalize();
	    push(@$meals, $current_meal);
	}
	$current_meal = $class->new(date => $current_date,
				    meal => $current_meal_name);
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
	    $register_meal->();
	}
	elsif (/(breakfast|lunch|snack|dinner):$/i) {
	    $current_meal_name = lc($1);
	    $register_meal->();
	}
	elsif ($class->parse_units($_)) {
	    my ($amount, $units, $unit_class, $name) = $class->parse_units($_);
	    my $item = Food::Item->fetch_item($name);
	    if (! $item) {
		warn "Can't find '$name' to add it to the current meal.\n";
		# Make a fake item to record our uncertainty.
		$item = Food::Item->new(name => $name);
	    }
	    $current_meal->add_ingredient($item, $amount, $units);
	}
	elsif (my $item = Food::Item->fetch_item($_)) {
	    $current_meal->add_ingredient($item, 1, 'serving');
	}
	elsif (/random/i) {
	    # Don't bother with these.
	}
	else {
	    warn "Can't find '$_' to add it to the current meal.\n";
	    # Make a fake item to record our uncertainty.
	    $item = Food::Item->new(name => $_);
	    $current_meal->add_ingredient($item, 1, 'serving');
	}
    }
    $register_meal->();
    return $meals;
}
