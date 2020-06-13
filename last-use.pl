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
use Food::Item;

my $recipe_file_name;
my $by_recipe_p;
GetOptions('recipe-file=s' => \$recipe_file_name,
	   'by-recipe!' => \$by_recipe_p);
die "$0:  No meals files named on the command line.\n"
    unless $by_recipe_p || @ARGV;

# Read the item/recipe database.
Food::Item->parse_recipes($recipe_file_name)
    if $recipe_file_name;

# Read the meal files.
for my $file (@ARGV) {
    # Find meals.
    my $meals = Food::Meal->parse_meals($file);
    for my $meal (@$meals) {
	$meal->mark_last_use($file);
    }
}

# Collect recipes by file last used.
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
if ($by_recipe_p) {
    # Look for "interesting" recipes.
    while (<>) {
	chomp;
	my ($file, $recipe_name) = split("\t");
	my $recipe = Food::Item->fetch_item($recipe_name);
	if (! $recipe) {
	    warn "$0:  Can't find '$recipe_name', ignoring.\n";
	}
	else {
	    $recipe->{_interesting_p}++;
	}
    }
    for my $item (sort { $a->file_name cmp $b->file_name
			     || $a->line_number <=> $b->line_number;
		  } values(%Food::Item::item_from_name)) {
	if ($item->{_interesting_p}) {
	    my $item_name = $item->name;
	    my $type = $item->isa('Food::Recipe') ? 'recipe' : 'item';
	    print($item->file_name, ':', $item->line_number,
		  ":[$type $item_name]\n");
	    # We have to reset this because there will be duplicates in the
	    # item hash due to aliases.
	    $item->{_interesting_p} = 0;
	}
    }
}
else {
    for my $last_use (sort(keys(%recipes_from_last_use))) {
	my $recipes = $recipes_from_last_use{$last_use};
	for my $recipe (sort { $a->name cmp $b->name; } @$recipes) {
	    print(join("\t", $last_use, $recipe->name), "\n");
	}
    }
}

__END__

=head1 NAME

last-use.pl -- find the file in which a recipe was iast referenced

=head1 SYNOPSIS

    last-use.pl [ --help | --man | --usage ]

=head1 DESCRIPTION

Given a list of menu files (see C<meals.pl>) in chronological order,
C<last-use.pl> collects all of the food items referenced by these
files, and produces a tab-delimited list of file name and food item,
sorted by menu file.

=head1 OPTIONS

As with other C<Getopt::Long> scripts, option names can be abbreviated
to anything long enough to be unambiguous.

=over 4

=item B<--recipe-file>

Specifies a default recipe file to read before reading menu files on
the command line.  If none is specified, no recipe file is preread.

=back

=head1 EXAMPLES

To find all last used files in a set of meal files in the F<food>
directory, a sibling of F<meals>:

	cd ../food
	perl -Mlib=../meals ../meals/last-use.pl \
		--recipe-file recipes.text *-food.text

The output generated is of the form:

	1412-food.text	Jan's chickpeas and stars
	1412-food.text	spiced spinach with "paneer"
	1501-food.text	Boca spaghetti sauce
	1501-food.text	Indian cauliflower dish
	1501-food.text	Jan's feta pizza
	1501-food.text	broccoli-potato soup with fresh herbs
	1501-food.text	chick-pea-walnut cream
	1501-food.text	chorizo potato soup
	1501-food.text	linguine with tomato curry
	1501-food.text	pea soup with red peppers
	1501-food.text	potato leek soup with lentils
	1501-food.text	spiced crumbles with peas
	1501-food.text	stir-fried veggies 1
	1501-food.text	stir-fried veggies 2
	1501-food.text	three bean chili burger
	1501-food.text	zucchini and pine nuts
	1502-food.text	Brooksetta burgers
	1502-food.text	Jan's bean wombat
	1502-food.text	Jan's spaghetti sauce
	1502-food.text	black-eyed peas and greens

This assumes all files are named something like F<1706-food.text>, or
something similar that sorts alphanumerically by date;
"F<*-food.text>" enumerates them on the command line alphanumerically,
and C<last-use.pl> will therefore process them chronologically.

The previous example is good for finding a good cutoff point for
breaking down a large F<recipes.text> file, because the output is also
in chronlogical order.  For actually splitting up the file, what is
wanted is to see the older recipes in the order they appear in
F<recipes.text>.  For this you need to feed the results of the first
C<last-use.pl> application back again using the C<--by-recipe> option.

Suppose it is decided that all recipes that were not used after 2016
are to be split out into a new recipe file.  You can then filter the
previous C<last-use.pl> invocation through C<grep> to select the older
files, and pipe the result to C<last-use.pl> again:

	perl -Mlib=../meals ../meals/last-use.pl \
		--recipe-file ../meals/recipes.text *-food.text \
	    | grep '^1[456]..-food' \
	    | perl -Mlib=../meals ../meals/last-use.pl --by-recipe \
		--recipe-file ../meals/recipes.text

The output from the second C<last-use.pl> looks like something C<grep>
would produce:

	../meals/recipes.text:6:[recipe grilled tuna]
	../meals/recipes.text:11:[recipe spiced spinach with "paneer"]
	../meals/recipes.text:21:[recipe spiced spinach with "paneer" 2]
	../meals/recipes.text:33:[recipe Anna's spinach and mushrooms]
	../meals/recipes.text:43:[recipe Indian cauliflower dish]
	../meals/recipes.text:65:[recipe Jan's chickpeas and stars]
	../meals/recipes.text:77:[recipe spiced crumbles with peas]
	../meals/recipes.text:87:[recipe Anna's Boudin sourdough garlic bread]
	../meals/recipes.text:100:[recipe broccoli-potato soup with fresh herbs]
	../meals/recipes.text:108:[recipe black beans in chipotle adobo sauce]

Note that you cannot pass F<1[456]*-food.text> as the command-line
argument to C<last-use.pl> in lieu of C<grep> posprocessing, because
if the earlier C<last-use.pl> does not see those later meal files, it
will come to different conclusions about when those meals were last
used.

=head1 BUGS

If you find any, please let me know.

=head1 AUTHOR

Bob Rogers E<lt>rogers@rgrjr.comE<gt>

=cut
