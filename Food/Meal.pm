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

1;
