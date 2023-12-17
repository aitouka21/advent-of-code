#!/usr/bin/env perl

while(<>) {
    %rgb= ();
    /(\d+) (\w)/ and $rgb{$2} < $1 and $rgb{$2} = $1 for split /[,;]/;
    $part1 += (/(\d+)/)[0] if $rgb{r} < 13 and $rgb{g} < 14 and $rgb{b} < 15;
    $part2 += $rgb{r} * $rgb{g} * $rgb{b};
}
print "($part1, $part2)\n";
