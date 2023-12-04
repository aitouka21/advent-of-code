#!/usr/bin/env perl

%card_counts = ();

while(<>) {
  %num_counts = ();

  my ($card_ptr) = /Card\s+(\d+)/;
  my $current_card_count = ++$card_counts{$card_ptr};

  $num_counts{$_}++ for (/:(.*)/)[0] =~ /(\d+)/g;

  my $winning_number_count = scalar(grep { $_ == 2 } values %num_counts);

  $sum += 1 << $winning_number_count - 1;
  $card_counts{++$card_ptr} += $current_card_count for (1..$winning_number_count);
}

$total += $_ for values %card_counts;
print "($sum, $total)\n";
