#!/usr/bin/env perl

use strict;
use warnings;
use ReDIF::Parser qw(redif_open_file redif_get_next_template_good_or_bad);
use JSON;

my $file = shift @ARGV or die "Usage: $0 file.rdf\n";

redif_open_file($file);

my $json = JSON->new->utf8->pretty;
print "[\n";
my $count = 0;
while (my $template = redif_get_next_template_good_or_bad()) {
    print ",\n" if $count++;
    print $json->encode($template);
}
print "\n]\n";

