#!/usr/bin/perl
#
# Make a crude tally of main courses in a specified meal.
#
# [created.  -- rgr, 14-Dec-19.]
#

use strict;
use warnings;

use Getopt::Long;

use lib '.';

use Food::Meal;

my $recipe_file_name = 'recipes.text';
my $meal_name = 'dinner';

GetOptions('meal=s' => \$meal_name,
	   'recipe-file=s' => \$recipe_file_name);

# Read the item/recipe database.
Food::Item->parse_recipes($recipe_file_name);

# Read the meal files.
my $n_meals = 0;
my %tally_from_name;
for my $file (@ARGV) {
    # Find meals.
    my $meals = Food::Meal->parse_meals($file);
    for my $meal (@$meals) {
	next
	    unless $meal->meal eq $meal_name;

	# Find the "main course."  This will be a recipe over an ingredient,
	# and the one with the greater number of calories if there is more than
	# one of a kind.
	my $ingredients = $meal->ingredients;
	my $main_course = $ingredients->[0];
	for my $ingredient (@{$meal->ingredients}) {
	    $main_course = $ingredient
		if $ingredient->better_main_course_p($main_course);
	}
	my $item = $main_course->item;
	# print $meal->date, ":  $item => ", $item->name, "\n";
	$tally_from_name{$item->name}++;
	$n_meals++;
    }
}
for my $name (sort(keys(%tally_from_name))) {
    printf("%7d\t%s\n", $tally_from_name{$name}, $name);
}
