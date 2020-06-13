#================
#
# A meal file is a collection of meals that come from a particular input file.
#

use strict;
use warnings;

package Food::MealFile;

use Food::Item;

use base qw(Food::Base);

BEGIN {
    Food::MealFile->define_class_slots(qw(file_name summary_item meals));
}

sub produce_summaries {
    # Generate summary output and/or return a list of daily summary items.
    my ($self, %keywords) = @_;
    my $detail_level = $keywords{detail_level} || 'none';
    my $need_day_totals = $keywords{need_day_totals} || 0;
    my $file_summary_p = $keywords{file_summary_p} || 0;
    $need_day_totals = 1
	unless $detail_level eq 'none';

    my @slots = qw(net_carbohydrate_grams fat_grams protein_grams calories);
    my @day_totals;
    my $produce_day_total = sub {
	# Wrap up the last day total, producing output and/or pushing it onto
	# @day_totals as appropriate.
	my ($day_total) = @_;

	return
	    unless $day_total;
	push(@day_totals, $day_total)
	    if $need_day_totals;
	$day_total->present_summary(detailed_p => $detail_level eq 'item',
				    display_cho_p => 1)
	    if $detail_level ne 'none';
    };

    # Generate output.
    my ($day_total, $current_day);
    my $summary = $self->summary_item;
    if (! $summary && $file_summary_p) {
	$summary = Food::Item->new(name => $self->file_name . ' total:');
	$self->summary_item($summary);
    }
    for my $meal (@{$self->meals}) {
	if ($need_day_totals
	    && (! $day_total || $current_day ne $meal->date)) {
	    $produce_day_total->($day_total);
	    $day_total = Food::Item->new(name => $meal->date . ' total:');
	    $current_day = $meal->date;
	}
	$meal->present_summary(detailed_p => $detail_level eq 'item',
			       colon_p => 1,
			       display_cho_p => 1)
	    if $detail_level eq 'meal' || $detail_level eq 'item';
	for my $slot (@slots) {
	    my $value = $meal->$slot();
	    next
		unless defined($value);
	    if ($file_summary_p) {
		my $slot_value = $summary->$slot() || 0;
		$summary->$slot($slot_value + $value);
	    }
	    if ($need_day_totals) {
		my $slot_value = $day_total->$slot() || 0;
		$day_total->$slot($slot_value + $value);
	    }
	}
    }
    $produce_day_total->($day_total);
    $summary->present_summary(display_cho_p => 1)
	if $file_summary_p;
    return @day_totals;
}

1;
