#!/usr/bin/env perl

use strict;
use warnings;

sub process {
    my %p1_s = ( r => 12, g => 13, b => 14 );
    my %p2_s = ( r => 0,  g => 0,  b => 0 );
    my ($game_id) = $_[0] =~ /Game (\d+)/;
    my $p1_valid = 1;
    while ( $_[0] =~ /(\d+) (\w)/g ) {
        $p1_valid = $p1_s{$2} >= $1 if $p1_valid;
        $p2_s{$2} = $1 if $p2_s{$2} < $1;
    }
    return ( $p1_valid ? $game_id : 0, $p2_s{r} * $p2_s{g} * $p2_s{b} );
}

open my $fh, '<', 'input.txt';
my ( $p1_sum, $p2_sum ) = ( 0, 0 );
while (<$fh>) {
    my ( $p1, $p2 ) = process($_);
    $p1_sum += $p1;
    $p2_sum += $p2;
}
print "Part 1: $p1_sum, Part 2: $p2_sum\n";
