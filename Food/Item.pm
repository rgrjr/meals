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

BEGIN {
    Food::Item->define_class_slots
	(qw(name serving_size_g serving_size_ml last_use
	    protein_grams fat_grams carbohydrate_grams
	    fiber_grams calories cholesterol_mg sodium_mg),
	 # Where defined.
	 qw(file_name line_number));
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
    if (length($name) > 38) {
	# Print the name on its own line so as not to shift the other columns.
	print $name, "\n";
	$name = '';
    }
    printf('%-38s', $name);
    for my $slot (qw(net_carbohydrate_grams fat_grams
		     protein_grams calories)) {
	my $value = $self->$slot();
	$value *= $n_servings
	    if defined($value);
	print ' ', $self->show_total($value, ! defined($value));
    }
    if ($keys{display_cho_p}) {
	my $cho_percent = $self->carbohydrate_percent;
	printf(" CHO%%%.1f", $cho_percent)
	    if defined($cho_percent);
    }
    elsif ($n_servings != 1) {
	printf "  %3.2fsvg", $n_servings;
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

# Remember files by their device and inode numbers so we have a solid handle on
# whether we've seen it before, regardless of relative file naming, links, etc.
my %file_from_dev_and_inode;

sub parse_recipes {
    my ($class, $file_name) = @_;
    require Food::Recipe;

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
	my ($name, $item, $file, $line) = @_;

	my $lc_name = lc($name);
	my $existing_item = $item_from_name{$lc_name};
	if ($existing_item) {
	    $warning->("Conflict for '$name'.\n");
	}
	else {
	    $item_from_name{$lc_name} = $item;
	    if ($file) {
		$item->file_name($file);
		$item->line_number($line);
	    }
	}
    };

    my $parse_file;
    $parse_file = sub {
	my ($file_name, $base_file_name) = @_;

	# Open the file, and check if we've seen it before.
	my $stream = $open_file->($file_name, $base_file_name);
	if (! $stream) {
	    $warning->("Can't open '$file_name':  $!");
	    return;
	}
	my ($dev, $inode) = stat($stream);
	my $key = ($inode
		   ? "$dev:$inode"
		   # Probably we're running on a non-POSIX system, so try to
		   # fake it with just the file name.
		   : $file_name);
	return
	    if $file_from_dev_and_inode{$key};
	$file_from_dev_and_inode{$key} = $file_name;
	push(@include_stack, [ $stream, $file_name ]);

	# It's new, so parse the thing.
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
		    $store_item->($name, $current_item, $file_name,
				  $stream->input_line_number);
		}
		elsif ($type eq 'recipe') {
		    $current_item = Food::Recipe->new(name => $name);
		    $store_item->($name, $current_item, $file_name,
				  $stream->input_line_number);
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

    # This is the "bar" we must pass:  A match to all recipe names and
    # ingredients.
    my $bar = @$name_regexps + @$ingredient_regexps;
    my @recipes = sort { $a->name cmp $b->name;
    } grep {
	if ($_->isa('Food::Recipe')) {
	    my $match_count = 0;
	    my $name = $_->name;
	    for my $re (@$name_regexps) {
		$match_count++
		    if $name =~ /$re/i;
	    }
	    my $ingredients = $_->ingredients;
	    for my $ingredient (@$ingredients) {
		my $name = $ingredient->item->name;
		for my $re (@$ingredient_regexps) {
		    $match_count++
			if $name =~ /$re/i;
		}
	    }
	    $match_count == $bar;
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

sub _sane_amount {
    my ($amount, $units) = @_;

    return ''
	unless defined($amount) && length($amount);
    return $amount . $units
	unless $amount =~ /^(.*[.])(.*)$/;
    # Truncate the decimal fraction to a reasonable length.
    my ($whole_part, $fraction) = $amount =~ //;
    return $whole_part . $units
	if $units eq 'g';
    my $max_digits = $units eq 'C' || $units eq 'lb' ? 3 : 2;
    return $amount . $units
	if length($fraction) <= $max_digits;
    return $whole_part . substr($fraction, 0, $max_digits) . $units;
}

sub show_item_details {
    my ($self) = @_;

    $self->present_summary(display_cho_p => 1);
    my $ingredients = $self->ingredients;
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
	my $units = $ing->units || '';
	if ($units eq 'serving') {
	    $units = '';
	}
	elsif ($units eq 'recipe') {
	    $units = ' recipe';
	}
	my $display = _sane_amount($ing->amount, $units) . ' ' . $it->name;
	if ($it->serving_size_g) {
	    my $grams = $n_svg * $it->serving_size_g;
	    $total_weight += $grams;
	    $display .= sprintf(' (%dg)', $grams);
	}
	else {
	    $missing_weight_p++;
	    $missing_p = '*';
	}
	printf(" %s  %-33s  %s %s %s %s CHO%%%.1f\n",
	       $missing_p || ' ', $display,
	       $self->show_total($carbs), $self->show_total($fat),
	       $self->show_total($protein),
	       $self->show_total($calories), $cho_pct);
    }
    if ($total_weight) {
	my $item_grams = $self->serving_size_g;
	printf("  Declared serving size:  %dg\n", $item_grams)
	    if $item_grams;
	my $n_servings = $self->n_servings;
	if (! $n_servings) {
	    print "  Assuming one serving.\n";
	    $n_servings = 1;
	}
	my $theo_grams = $total_weight / $n_servings;
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

1;
