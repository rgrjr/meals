#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use IO::File;

use lib '.';

use Food::Meal;

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
    my $ingredients = $item->ingredients;
    next
	unless $ingredients;
    my $missing_weight_p = 0;
    my $total_weight = 0;
    my $n_ingredients = @$ingredients;
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
	my $missing_p;
	if ($it->serving_size_g) {
	    $total_weight += $n_svg * $it->serving_size_g;
	}
	else {
	    $missing_weight_p++;
	    $missing_p = '*';
	}
	my $units = $ing->units || '';
	if ($units eq 'serving') {
	    $units = '';
	}
	elsif ($units eq 'recipe') {
	    $units = ' recipe';
	}
	printf(" %s  %-28s  %s %s %s %s CHO%%%.1f\n",
	       $missing_p || ' ',
	       ($ing->amount || '') . $units . ' ' . $it->name,
	       $item->show_total($carbs), $item->show_total($fat),
	       $item->show_total($protein),
	       $item->show_total($calories), $cho_pct);
    }
    if ($total_weight) {
	my $item_grams = $item->serving_size_g;
	printf("  Declared serving size:  %dg\n", $item_grams)
	    if $item_grams;
	my $theo_grams = $total_weight / $item->n_servings;
	if ($missing_weight_p) {
	    printf("  Theoretical serving size:  %dg"
		   . " (missing %d out of %d weights)\n",
		   $theo_grams, $missing_weight_p, $n_ingredients);
	}
	else {
	    printf("  Theoretical serving size:  %dg\n", $theo_grams);
	}
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
