###================
#
# An ingredient is a particular food item eaten during a particular meal, or
# added to a particular recipe, both with a specified serving amount.
#

use strict;
use warnings;

package Food::Ingredient;

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
    elsif ($units eq 'recipe') {
	$self->n_servings($amount * $item->n_servings);
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

sub mark_last_use {
    my ($self, $last_use) = @_;

    my $item = $self->item;
    $item->last_use($last_use)
	if $item;
}

sub calories {
    my ($self) = @_;

    return $self->n_servings * $self->item->calories;
}

# Support for tally-dinners.pl.
sub better_main_course_p {
    my ($self, $other) = @_;

    my $self_item_type = ref($self->item);
    my $other_item_type = ref($other->item);
    return 1
	if ($self_item_type eq 'Food::Recipe'
	    && $other_item_type ne 'Food::Recipe');
    return 0
	if ($other_item_type eq 'Food::Recipe'
	    && $self_item_type ne 'Food::Recipe');
    return $self->calories > $other->calories;
}

1;
