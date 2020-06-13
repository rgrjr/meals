###================
#
# A meal is a collection of ingredients eaten together (loosely) on a
# particular date.
#

use strict;
use warnings;

package Food::Meal;

use Food::Item;

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

    open(my $stream, '<', $file_name)
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
	elsif (/^\s*include\s*(\S+)\s*$/) {
	    # Include a recipe file.
	    Food::Item->parse_recipes($1);
	}
	elsif ($class->parse_units($_)) {
	    my ($amount, $units, $unit_class, $name) = $class->parse_units($_);
	    my $item = Food::Item->fetch_item($name);
	    if (! $item) {
		warn("$file_name:$.:Can't find '$name' ",
		     "to add it to the current meal.\n");
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
	    warn("$file_name:$.:Can't find '$_' ",
		 "to add it to the current meal.\n");
	    # Make a fake item to record our uncertainty.
	    $item = Food::Item->new(name => $_);
	    $current_meal->add_ingredient($item, 1, 'serving');
	}
    }
    $register_meal->();
    return $meals;
}

sub plot_meal_data {
    # Generate plots using Gnuplot.
    my ($class, $day_totals, $cho_pct_plot_file, $calorie_plot_file) = @_;
    die "bug"
	unless $cho_pct_plot_file || $calorie_plot_file;
    # Generate a dummy file from the other if only one is wanted.
    # $cho_pct_plot_file ||= $calorie_plot_file . '.dummy';
    # $calorie_plot_file ||= $cho_pct_plot_file . '.dummy';

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
	for my $day_total (@$day_totals) {
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

    # Generate the plots.
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

1;
