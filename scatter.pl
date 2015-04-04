#!/usr/bin/perl
#
# Produce a scatterplot of weight change as a function of calorie intake.
#
# $Id$

use strict;
use warnings;

use lib '/shared.local/mgi/modest';

use Getopt::Long;
use Date::Parse;
use ModGen::Statistics;

use constant SECONDS_PER_DAY => 24 * 60 * 60;

my ($calorie_file, $weight_file);
GetOptions('weight=s' => \$weight_file,
	   'calories=s' => \$calorie_file);
$calorie_file ||= shift(@ARGV)
    or die "$0:  Missing --calories file name.\n";
$weight_file ||= shift(@ARGV)
    or die "$0:  Missing --weight file name.\n";

### Main code.

# First, find each day's calories.  This is just meals.pl output.
my (%calories_from_day, $first_day);
{
    open(my $in, '<', $calorie_file)
	or die "$0:  Can't open --calories file '$calorie_file'.\n";
    while (<$in>) {
	chomp;
	my ($heading, $carbs, $fat, $protein, $calories) = split("\t");
	next
	    unless $heading =~ /^(\S+) total:/;
	my $date = str2time($1);
	next
	    unless $date;
	my $day = int($date/SECONDS_PER_DAY);
	$first_day = $day
	    if ! $first_day || $day < $first_day;
	$calories =~ s/ CHO%.*//;
	$calories =~ s/ +//g;
	$calories_from_day{$day} = $calories;
    }
}

# Now read the weights, and compute the deltas.
my %weight_change_from_day;
{
    my ($next_day, $next_weight);
    open(my $in, '<', $weight_file)
	or die "$0:  Can't open --weights file '$weight_file'.\n";
    while (<$in>) {
	chomp;
	my ($raw_date, $weight) = split("\t");
	next
	    unless $raw_date && $weight;
	my $date = str2time($raw_date);
	next
	    unless $date;
	my $day = int($date/SECONDS_PER_DAY);
	last
	    # Use the fact that the weight file is most recent first.
	    if $day < $first_day;
	# warn "$raw_date => $weight\n";

	$weight_change_from_day{$day} = $next_weight - $weight
	    if $next_day && $day == $next_day - 1;
	($next_day, $next_weight) = ($day, $weight);
    }
}

# Produce results.
my $stats = ModGen::Statistics->new();
my $backward_stats = ModGen::Statistics->new();
for my $day (sort { $a <=> $b; } keys(%calories_from_day)) {
    my $calories = $calories_from_day{$day};
    my $weight_change = $weight_change_from_day{$day};
    next
	unless defined($calories) && defined($weight_change);
    print(join("\t", $calories, $weight_change), "\n");
    $stats->accumulate($calories, $weight_change);
    $backward_stats->accumulate($weight_change, $calories);
}
my ($slope, $intercept) = $stats->linear_regression;
my ($backward_slope, $backward_intercept) = $backward_stats->linear_regression;
warn("n=", $stats->n_samples, ", X intercept=$backward_intercept, ",
     "slope=$slope, Y intercept=$intercept\n");
