#!/usr/bin/perl
#
# Report the last use of a recipe.
#

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
my (@show_items, @recipes_matching, @ingredients_matching);
GetOptions('detailed!' => \$detailed_p,
	   'recipe-file=s' => \$recipe_file_name,
	   'recipes-matching=s' => \@recipes_matching,
	   'ingredients-matching=s' => \@ingredients_matching);
die "$0:  No meals files named on the command line.\n"
    unless @ARGV;

# Read the item/recipe database.
Food::Item->parse_recipes($recipe_file_name);

# Read the meal files.
my @all_meals;
for my $file (@ARGV) {
    # Find meals, and maybe ellipsize.
    my $meals = Food::Meal->parse_meals($file);
    for my $meal (@$meals) {
	$meal->mark_last_use($file);
	push(@all_meals, $meal);
    }
}

# Collect recipes by date last used.
my (%recipes_from_last_use, %seen_recipe_p);
for my $recipe (values(%Food::Item::item_from_name)) {
    next
	if $seen_recipe_p{$recipe->name}++;
    next
	unless $recipe->isa('Food::Recipe');
    my $last_use = $recipe->last_use;
    push(@{$recipes_from_last_use{$last_use}}, $recipe)
	if $last_use;
}

# Generate output.
for my $last_use (sort(keys(%recipes_from_last_use))) {
    my $recipes = $recipes_from_last_use{$last_use};
    for my $recipe (sort { $a->name cmp $b->name; } @$recipes) {
	print(join("\t", $last_use, $recipe->name), "\n");
    }
}
