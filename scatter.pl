#!/usr/bin/perl
#
# Produce a scatterplot of weight change as a function of calorie intake.

use strict;
use warnings;

use Getopt::Long;
use Date::Parse;
use ModGen::Statistics;

use constant SECONDS_PER_DAY => 24 * 60 * 60;

sub five_digits {
    my ($value) = @_;

    if ($value =~ /^(-?)(\d*)\.(\d+)$/) {
	my ($sign, $int, $frac) = $value =~ //;
	my $frac_digits = 5 - length($int);
	if ($frac_digits <= 0) {
	    return "$sign$int";
	}
	else {
	    $frac_digits += length($1)+length($int)
		# Leading zeros don't count.
		if ! $int && $frac =~ /^(0*)/;
	    return "$sign$int." . substr($frac, 0, $frac_digits);
	}
    }
    else {
	# integer or E-notation; punt.
	return $value;
    }
}

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
die "$0:  No days in --calories file '$calorie_file'??\n"
    unless $first_day;

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

# Produce the raw data.
my $stats = ModGen::Statistics->new();
my $backward_stats = ModGen::Statistics->new();
my $out_file_name = "scatter-$$.tmp";
{
    open(my $out, '>', $out_file_name)
	or die "$0:  bug:  can't open temp file '$out_file_name':  $!";
    for my $day (sort { $a <=> $b; } keys(%calories_from_day)) {
	my $calories = $calories_from_day{$day};
	my $weight_change = $weight_change_from_day{$day};
	next
	    unless defined($calories) && defined($weight_change);
	print $out (join("\t", $calories, $weight_change), "\n");
	$stats->accumulate($calories, $weight_change);
	$backward_stats->accumulate($weight_change, $calories);
    }
}

# Produce the plot.
my ($slope, $intercept) = $stats->linear_regression;
my ($backward_slope, $backward_intercept) = $backward_stats->linear_regression;
my $prefix = "n=" . $stats->n_samples;
warn("$prefix, X intercept=$backward_intercept (cal), ",
     "slope=$backward_slope (cal/lb),\n", ' ' x (2+length($prefix)),
     "Y intercept=$intercept (lb), slope=$slope (lb/cal)\n");
{
    open(my $out, '| gnuplot')
	or die "$0:  Couldn't pipe to gnuplot:  $!";
    print $out "set term png\n";
    print $out ("plot [1000:4000] [-2.5:2.5] '$out_file_name',",
		(map { five_digits($_);
		 } 'x*', $slope, '+', $intercept, ', (x-',
		    $backward_intercept, ')/', $backward_slope),
		"\n");
}
unlink($out_file_name);
