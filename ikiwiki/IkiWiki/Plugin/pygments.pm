#!/usr/bin/perl
# Syntax highlighter using pygments

package IkiWiki::Plugin::pygments;

use warnings;
use strict;
use IkiWiki 3.00;


my $command = $config{pygments_cmd} || "/usr/local/bin/pygmentize";

sub import {
    # [[!pygments]] preprocessor directive for inline code
    hook(type => "preprocess", id => "pygments", call => \&preprocess);
}

sub highlight (@) {
    my %params = @_;
    my $input = $params{content};
    chomp $input;

    eval q{use Carp qw( croak );};
    eval q{use IPC::Run qw( run );};
    eval q{use IPC::Open2};

    local(*SPS_IN, *SPS_OUT);  # Create local handles

    my $style = 'style=monokai';

    # If the number="yes" is used in the preprocessor directive,
    # request line numbering and use spaces instead of zero padding
    # for the numbers.
    if ($params{linenos} and $params{linenos} eq "yes") {
        $style .= ',linenos=1';
    }

    my @cmd = ('-O', $style, '-f', 'html', '-g');

    if ($params{lexer} and $params{lexer} ne "") {
        push @cmd, '-l';
        push @cmd, $params{lexer};
    }

    my $pid = open2(*SPS_IN, *SPS_OUT, $command, @cmd);
    error("Unable to open $command") unless $pid;

    print SPS_OUT $input;
    close SPS_OUT;

    my @html = <SPS_IN>;
    close SPS_IN;

    waitpid $pid, 0;
    return @html;
}


sub preprocess (@) {
    my %params = @_;
    my @html = highlight(%params);

    my $content = '<div class="highlight-pygments">'
         . "\n" . join('', @html) . "\n"
        . "</div>\n";

    # Workaround for https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=487397
    $content =~ s/\[/&#91;/g;
    # Workaround for perl bug (#376329)
    require Encode;
    $content = Encode::decode_utf8($content);
    return $content;
}

1

__END__

=head1 NAME

ikiwiki Plug-in: code

=head1 SYNOPSIS

Inline syntax highlighting via pygments. Pretty much ripped off Jason Blevins
code. Left his name in copyright.

I actually use pandoc now, so I this is unmaintained as of 2016-09-28.

=head1 AUTHORS

Tyler Cipriani <tyler@tylercipriani.com>, http://tylercipriani.com

=head1 SEE ALSO

http://jblevins.org/projects/ikiwiki/code

=head1 LICENSE

Copyright (C) 2008 Jason Blevins
Copyright (C) 2016 Tyler Cipriani

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
