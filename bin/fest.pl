#!/usr/bin/env perl
# Via brennen in irc
use warnings;
use strict;
use JSON qw( decode_json );
while (<>) {
  print "$_";
  open my $festival, "| festival --tts" or die "can't fork: $!";
  print $festival $_;
  close $festival;
}
