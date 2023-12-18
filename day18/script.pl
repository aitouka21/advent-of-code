#!/usr/bin/env perl

use strict;
use warnings;

my ($x, $y, $b, $sum, @pts) = (0, 0, 0, 0, ([0,0]));

while(<>) {
  my ($step, $dir) = /#(\w+)(0|1|2|3)/;

  $b += ($step = hex($step));
  
  push @pts, [$x += $step, $y] if $dir eq 0;
  push @pts, [$x -= $step, $y] if $dir eq 2;
  push @pts, [$x, $y += $step] if $dir eq 3;
  push @pts, [$x, $y -= $step] if $dir eq 1;
}

for (my $i = -1; $i < $#pts; ++$i) {
  $sum += $pts[$i+1][0] * $pts[$i][1] - $pts[$i][0] * $pts[$i+1][1];
}

print abs($sum) / 2 + $b / 2 + 1, "\n";
