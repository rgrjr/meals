#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use IO::File;

use lib '.';

use Food::Meal;
use Food::MealFile;

my $detail_level;
my $file_p = 0;
my $tail_length = 0;
my @recipe_files;
my $calorie_plot_file = '';
my $cho_pct_plot_file = '';
my (@show_items, @recipes_matching, @ingredients_matching);
GetOptions('detail-level=s' => \$detail_level,
	   'file!' => \$file_p,
	   'recipe-file=s' => sub {
	       Food::Item->parse_recipes($_[1]);
	   },
	   'plot-cho-percent=s' => \$cho_pct_plot_file,
	   'show-item=s' => \@show_items,
	   'tail=i' => \$tail_length,
	   'recipes-matching=s' => \@recipes_matching,
	   'ingredients-matching=s' => \@ingredients_matching,
	   'plot-calories=s' => \$calorie_plot_file);
my $search_p = @recipes_matching || @ingredients_matching;
my $plot_p = $calorie_plot_file || $cho_pct_plot_file;
$detail_level ||= $plot_p || @show_items || $search_p ? 'none' : 'meal';
die "$0:  --detail-level must be one of 'none', 'day', 'meal', or 'item'.\n"
    unless $detail_level =~ /^(none|day|meal|item)$/;

# Read the meal files.
unshift(@ARGV, '-')
    unless @ARGV || @show_items;
my @slots = qw(net_carbohydrate_grams fat_grams protein_grams calories);
my @file_totals;
for my $file (@ARGV) {
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
    push(@file_totals, 
	 Food::MealFile->new(file_name => $file,
			     meals => $meals))
	if @$meals;
}

### Execution.

# Look for matching recipes.
if ($search_p) {
    Food::Item->show_matching_recipes(\@recipes_matching,
				      \@ingredients_matching);
}

# Do items we've been asked to show.
for my $item_name (@show_items) {
    my $item = Food::Item->fetch_item($item_name);
    if (! $item) {
	warn "$0:  Can't find '$item_name' in our database.\n";
    }
    else {
	$item->show_item_details();
    }
}
exit(0)
    unless  $detail_level ne 'none' || $plot_p || $file_p;

# Generate output.
my $need_day_total_p = $detail_level ne 'none' || $plot_p;
my @day_totals;
for my $meal_file (@file_totals) {
    push(@day_totals,
	 $meal_file->produce_summaries(detail_level => $detail_level,
				       need_day_totals => $need_day_total_p,
				       file_summary_p => $file_p));
}

# Produce a calorie plot if requested.
Food::Meal->plot_meal_data(\@day_totals, $cho_pct_plot_file,
			   $calorie_plot_file)
    if $plot_p;
