#!/usr/bin/env perl

while(<>) {
    %rgb= ();
    ($game_id) = /(\d+)/;    
    /(\d+) (\w)/ and $rgb{$2} < $1 and $rgb{$2} = $1 for split /[,;]/;

    $part1 += $game_id if $rgb{r} < 13 and $rgb{g} < 14 and $rgb{b} < 15;
    $part2 += $rgb{r} * $rgb{g} * $rgb{b};
}
print "($part1, $part2)\n";
