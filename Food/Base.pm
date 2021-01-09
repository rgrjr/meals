###================
#
# Base class for food objects.
#

package Food::Base;

use strict;
use warnings;

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

my (%weight_in_grams, %volume_in_ml, %unit_alias);
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
    %unit_alias
	= (cup => 'C',
	   ounce => 'oz',
	   pound => 'lb',
	   teaspoon => 'tsp',
	   teasp => 'tsp',
	   tablespoon => 'T',
	   tbsp => 'T');
}

sub _canonicalize_units {
    # The word is left unchanged if not a unit.
    my ($units) = @_;

    if ($units =~ /s$/i) {
	my $nonplural_alias = $unit_alias{substr(lc($units), 0, -1)};
	$units = $nonplural_alias
	    if $nonplural_alias;
    }
    elsif ($unit_alias{$units}) {
	$units = $unit_alias{$units};
    }
    return $units;
}

sub weight_units_p {
    my ($class, $units) = @_;

    $units = _canonicalize_units($units);
    return $weight_in_grams{$units};
}

sub volume_units_p {
    my ($class, $units) = @_;

    $units = _canonicalize_units($units);
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
    elsif ($string =~ m@^\s*(-?\.\d+|-?\d+\.\d*)\s*(.*)@) {
	# Integer or decimal fraction.
	($amount, $unit_string) = ($1, $2);
    }
    elsif ($string =~ m@^\s*(-?\d+)\s*(.*)@) {
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
	$units = _canonicalize_units($units);
	if ($units =~ /^(svg|serving)s?$/i) {
	    $unit_class = $units = 'serving';
	}
	elsif ($units =~ /^recipes?$/i) {
	    $unit_class = $units = 'recipe';
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

1;
