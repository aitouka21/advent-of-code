#!/usr/bin/env perl

%instances = ();

while(<>) {
  $c = 0;
  %h = ();
  ($card) = /Card\s+(\d+)/;
  $n_of_card = ++$instances{$card};
  $h{$_}++ for (/(?:[:])(.*)/)[0] =~ /(\d+)/g;
  $_ == 2 and $c++ for values %h;
  $sum += ($c ? 2 ** ($c - 1) : 0);
  $instances{++$card}+=$n_of_card for (1..$c);
}
$total += $_ for values %instances;
print "($sum, $total)\n";
