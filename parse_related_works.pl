#!/usr/bin/env perl
use strict;
use warnings;
use utf8;
use JSON::PP;    # MUST be JSON::PP, not JSON

# Path to relatedworks.dat passed by R
my $file = shift @ARGV
  or die "Usage: $0 /path/to/relatedworks.dat\n";

# Prepare the namespace used in the data file
package EP::Data;
our %relatedworks;

# Return to main to execute the file
package main;

# Load the data file
do $file or die "Cannot load $file: $@";

# Make sure the hash exists
my $href = \%EP::Data::relatedworks;

# Convert to JSON using JSON::PP ONLY
my $json = JSON::PP->new
                   ->utf8
                   ->canonical
                   ->pretty
                   ->encode($href);

print $json;
