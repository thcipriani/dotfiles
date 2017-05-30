#!/usr/bin/env perl

package IkiWiki::Plugin::s3cmd;

use strict;
use warnings;
use IkiWiki 3.00;

my $s3script = $config{s3cmd_script} || $ENV{'HOME'} . '/.dotfiles.git/hooks/post-update.d/99-amazon-update';
my $s3statepath = $config{s3cmd_state_path} || $ENV{'HOME'} . '/.ikiwiki/.s3cmd.HEAD';

sub import {
    hook(type => "savestate", id => "foo", call => \&needs_update);
}

sub needs_update() {
    chdir($config{srcdir});

    my $head = `git rev-parse HEAD`;
    chomp $head;

    local $/ = undef;
    open(my $fh, '<', $s3statepath)
        || warn('File cannot be read');

    my $cur = <$fh>;
    chomp $cur;

    close($fh);

    if ( $cur ne $head ) {
        open(my $fh, '>', $s3statepath)
            || die('Can\'t open state path');

        print $fh $head;
        system($s3script);
    }
}
