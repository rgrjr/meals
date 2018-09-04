###================
#
# Base class for recipes and meals, both of which have ingredients.
#

use strict;
use warnings;

package Food::HasIngredients;

use base qw(Food::Item);

use Food::Ingredient;

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

1;
