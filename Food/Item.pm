###================
#
# A food item has a name, a serving size (in terms of volume,
# weight, or both), and specific nutritional values per
# serving.  It can be a pure food item (e.g. an apple), a processed item with
# its nutritional values from the package, or (as a Food::Recipe) a collection
# of other food items with the sum of their nutritional values.
#

use strict;
use warnings;

package Food::Item;

use base qw(Food::Base);

use Food::Recipe;

BEGIN {
    Food::Item->define_class_slots
	(qw(name serving_size_g serving_size_ml last_use
	    protein_grams fat_grams carbohydrate_grams
	    fiber_grams calories cholesterol_mg sodium_mg));
}

use vars qw(%item_from_name);
%item_from_name = ();

my %message_and_units_from_name;
BEGIN {
    %message_and_units_from_name
	= (protein => [ qw(protein_grams g) ],
	   fat => [ qw(fat_grams g) ],
	   carbohydrate => [ qw(carbohydrate_grams g) ],
	   calories => [ 'calories', '' ],
	   fiber => [ qw(fiber_grams g) ],
	   cholesterol => [ qw(cholesterol_mg mg) ],
	   sodium => [ qw(sodium_mg mg) ]);
}

sub net_carbohydrate_grams {
    my $self = shift;

    if (@_) {
	$self->carbohydrate_grams(shift);
	$self->fiber_grams(0);
    }
    else {
	my $cho_grams = $self->carbohydrate_grams;
	return
	    unless defined($cho_grams);
	return $cho_grams - ($self->fiber_grams || 0);
    }
}

sub fetch_item {
    my ($class, $item_name) = @_;

    my $clean_name = $class->cleanup($item_name);
    my $item = $item_from_name{$clean_name};
    return $item
	if $item;
    return $item_from_name{$clean_name}
        if $clean_name =~ s/s$//;
}

sub add_ingredient {
    my ($self, $item, $amount, $units) = @_;

    warn($self->name, " is an item, not a recipe; can't add ",
	 $item->name, " to it.\n");
}

sub ingredients { }

sub carbohydrate_percent {
    my ($self) = @_;

    my $cho_grams = $self->net_carbohydrate_grams;
    my $calories = $self->calories;
    return
	unless defined($cho_grams) && defined($calories);
    return 100.0 * (4 * $cho_grams) / $calories;
}

sub present_summary {
    my ($self, %keys) = @_;
    my $n_servings = $keys{n_servings} || 1;

    my $name = $self->name;
    $name .= ':'
	if $keys{colon_p};
    $name = "  $name"
	if $keys{indent_name_p};
    printf('%-32s', $name);
    for my $slot (qw(net_carbohydrate_grams fat_grams
		     protein_grams calories)) {
	my $value = $self->$slot();
	$value *= $n_servings
	    if defined($value);
	print $self->show_total($value, ! defined($value));
    }
    if ($keys{display_cho_p}) {
	my $cho_percent = $self->carbohydrate_percent;
	printf(" CHO%%%.1f", $cho_percent)
	    if defined($cho_percent);
    }
    elsif ($n_servings != 1) {
	printf "\t%3.2fsvg", $n_servings;
    }
    print "\n";
    if ($keys{detailed_p} && $self->can('ingredients') && $self->ingredients) {
	for my $ingredient (@{$self->ingredients}) {
	    my $item = $ingredient->item;
	    my $n_servings = $ingredient->n_servings;
	    $ingredient->item->present_summary
		(n_servings => $ingredient->n_servings,
		 indent_name_p => 1);
	}
    }
}

sub parse_recipes {
    my ($class, $file_name) = @_;

    my $open_file = sub {
	my ($file_name, $base_file_name) = @_;

	my $stream = IO::File->new($file_name, '<');
	if (! $stream && $base_file_name && $base_file_name =~ m@(.+)/@) {
	    # Try something relative to $base_file_name.
	    my $local_file_name = "$1/$file_name";
	    $stream = IO::File->new($local_file_name, '<');
	}
	return $stream;
    };

    my @include_stack;		# [$stream, $name] for where included from.
    my $warning = sub {
	# Emit a warning prefixed by the @include_stack contents.

	for my $entry (@include_stack) {
	    my ($stream, $name) = @$entry;
	    warn "$0:  From $name line ", $stream->input_line_number, ":\n";
	}
	warn("$0:  ", @_);
    };

    my $store_item = sub {
	# Put the item in the %item_from_name hash under $name, warning about
	# any conflicts.
	my ($name, $item) = @_;

	my $lc_name = lc($name);
	my $existing_item = $item_from_name{$lc_name};
	if ($existing_item) {
	    $warning->("Conflict for '$name'.\n");
	}
	else {
	    $item_from_name{$lc_name} = $item;
	}
    };

    my $parse_file;
    $parse_file = sub {
	my ($file_name, $base_file_name) = @_;

	my $stream = $open_file->($file_name, $base_file_name);
	if (! $stream) {
	    $warning->("Can't open '$file_name':  $!");
	    return;
	}
	push(@include_stack, [ $stream, $file_name ]);

	my $current_item;
	while (<$stream>) {
	    chomp;
	    s/^\s+//;
	    if (! $_ || /^#/) {
		# Skip comments and blank lines.
	    }
	    elsif (/^include (.*)/) {
		my $include_file_name = $1;
		# warn "include_file_name $include_file_name";
		$current_item->finalize()
		    if $current_item && $current_item->can('finalize');
		$parse_file->($include_file_name, $file_name);
	    }
	    elsif (/^\[(\S+)\s+(.*)\]$/) {
		# Heading line.
		my ($type, $name) = //;
		$current_item->finalize()
		    if $current_item && $current_item->can('finalize');
		if ($type eq 'item') {
		    $current_item = Food::Item->new(name => $name);
		    $store_item->($name, $current_item);
		}
		elsif ($type eq 'recipe') {
		    $current_item = Food::Recipe->new(name => $name);
		    $store_item->($name, $current_item);
		}
		else {
		    $warning->("Unknown type '$type'.\n");
		    undef($current_item);
		}
	    }
	    elsif (! $current_item) {
		# Just ignore orphaned data.
	    }
	    elsif (/^serving size:\s*(.*)/) {
		my $size = $1;
		my ($amount, $units, $unit_class, $name)
		    = $class->parse_units("$size foo");
		if ($unit_class eq 'weight') {
		    my $weight = $class->convert($amount, $units, 1, 'g');
		    $current_item->serving_size_g($weight);
		}
		elsif ($unit_class eq 'volume') {
		    my $volume = $class->convert($amount, $units, 1, 'ml');
		    $current_item->serving_size_ml($volume);
		}
		else {
		    $warning->("Can't parse serving size '$size', for ",
			       $current_item->name, "\n.");
		}
	    }
	    elsif (/^servings:\s*(.*)/) {
		$current_item->n_servings($1);
	    }
	    elsif (/^alias:\s*(.*)/) {
		# Aliases are cheap.
		$store_item->($1, $current_item);
	    }
	    elsif ($class->parse_units($_)) {
		# Data value.
		my ($amount, $units, $unit_class, $name)
		    = $class->parse_units($_);
		my $mu = $message_and_units_from_name{$name};
		if ($mu) {
		    my ($message, $desired_units) = @$mu;
		    if (! $desired_units || $units eq $desired_units) {
			$current_item->$message($amount);
		    }
		    else {
			# [we could do better here.  -- rgr, 4-Apr-15.]
			die("$0:  Can't convert from $units ",
			    "to $desired_units.\n");
		    }
		}
		elsif (my $item = $class->fetch_item($name)) {
		    $current_item->add_ingredient($item, $amount, $units);
		}
		else {
		    $warning->("Don't know what to do with '$name'.\n");
		}
	    }
	    elsif (/^(source|nominal size):/) {
		# Ignore these for now.
	    }
	    elsif (my $item = $class->fetch_item($_)) {
		$current_item->add_ingredient($item, 1, 'serving');
	    }
	    else {
		$warning->("Don't know what to do with '$_'.\n");
	    }
	}
	$current_item->finalize()
	    if $current_item && $current_item->can('finalize');
	pop(@include_stack);
    };

    # Main code.
    $parse_file->($file_name);
}

sub show_matching_recipes {
    my ($class, $name_regexps, $ingredient_regexps) = @_;

    my @recipes = sort { $a->name cmp $b->name;
    } grep {
	if ($_->isa('Food::Recipe')) {
	    my $name = $_->name;
	    my $result = 0;
	    for my $re (@$name_regexps) {
		$result = 1, last
		    if $name =~ /$re/i;
	    }
	    my $ingredients = $_->ingredients;
	    if (! $result && $ingredients
		&& @$ingredients && @$ingredient_regexps) {
		for my $ingredient (@$ingredients) {
		    my $name = $ingredient->item->name;
		    for my $re (@$ingredient_regexps) {
			$result = 1, last
			    if $name =~ /$re/i;
		    }
		}
	    }
	    $result;
	}
    } values(%item_from_name);
    for my $recipe (@recipes) {
	$recipe->present_summary();
    }
}

sub mark_last_use {
    my ($self, $last_use) = @_;

    $self->last_use($last_use);
}

1;
