#!/usr/bin/env perl

package IkiWiki::Plugin::pandoc;

use warnings;
use strict;
use IkiWiki;
use FileHandle;
use IPC::Open2;
use File::Path qw/make_path/;
use JSON;

# activate with 'generate_$format' in meta; turn on all with 'generate_all_formats'.
my %extra_formats = (
    pdf    => { ext=>'pdf', label=>'PDF', format=>'latex', extra=>[], order=>1 },
    docx   => { ext=>'docx', label=>'DOCX', format=>'docx', extra=>[], order=>2 },
    odt    => { ext=>'odt', label=>'ODT', format=>'odt', extra=>[], order=>3 },
    beamer => { ext=>'beamer.pdf', label=>'Beamer', format=>'beamer', extra=>[], order=>4 },
    revealjs => { ext=>'revealjs.html', label=>'RevealJS', format=>'revealjs', extra=>['--self-contained'], order=>5 },
    epub   => { ext=>'epub', label=>'EPUB', format=>'epub3', extra=>[], order=>6 },
    latex  => { ext=>'tex', label=>'LaTeX', format=>'latex', extra=>['--standalone'], order=>7 },
);

sub import {
    my $markdown_ext = $config{pandoc_markdown_ext} || "mdwn";

    # May be both a string with a single value, a string containing commas or an arrayref
    if ($markdown_ext =~ /,/) {
        $markdown_ext = [split /\s*,\s*/, $markdown_ext];
    }

    hook(type => "getsetup", id => "pandoc", call => \&getsetup);
    hook(type => "pagetemplate", id => "pandoc", call => \&pagetemplate);
    hook(type => "pageactions", id => "pandoc", call => \&pageactions);

    if (ref $markdown_ext eq 'ARRAY') {
        foreach my $mde (@$markdown_ext) {
            hook(type => 'htmlize', id => $mde,
                 call => sub{ htmlize("markdown-markdown_in_html_blocks+raw_html", @_) });
        }
    } else {
        hook(type => "htmlize", id => $markdown_ext,
             call => sub { htmlize("markdown", @_) });
    }
    if ($config{pandoc_latex}) {
        hook(type => "htmlize", id => "tex",
             call => sub { htmlize("latex", @_) });
    }
    if ($config{pandoc_rst}) {
        hook(type => "htmlize", id => "rst",
             call => sub { htmlize("rst", @_) });
    }
    if ($config{pandoc_textile}) {
        hook(type => "htmlize", id => "textile",
             call => sub { htmlize("textile", @_) });
    }
    if ($config{pandoc_mediawiki}) {
        hook(type => "htmlize", id => "mediawiki",
             call => sub { htmlize("mediawiki", @_) });
    }
    if ($config{pandoc_opml}) {
        hook(type => "htmlize", id => "opml",
             call => sub { htmlize("opml", @_) });
    }
    if ($config{pandoc_org}) {
        hook(type => "htmlize", id => "org",
             call => sub { htmlize("org", @_) });
    }
}


sub getsetup () {
    return
    plugin => {
        safe => 1,
        rebuild => 1,
    },
    pandoc_command => {
        type => "string",
        example => "/usr/local/bin/pandoc",
        description => "Path to pandoc executable",
        safe => 1,
        rebuild => 0,
    },
    pandoc_citeproc => {
        type => "string",
        example => "/usr/local/bin/pandoc-citeproc",
        description => "Path to pandoc-citeproc executable",
        safe => 1,
        rebuild => 0,
    },
    pandoc_markdown_ext => {
        type => "string",
        example => "mdwn,md,markdown",
        description => "File extension(s) for Markdown files handled by Pandoc",
        safe => 1,
        rebuild => 1,
    },
    pandoc_latex => {
        type => "boolean",
        example => 0,
        description => "Enable Pandoc processing of LaTeX documents (extension=tex)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_rst => {
        type => "boolean",
        example => 0,
        description => "Enable Pandoc processing of reStructuredText documents (extension=rst)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_textile => {
        type => "boolean",
        example => 0,
        description => "Enable Pandoc processing of Textile documents (extension=textile)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_mediawiki => {
        type => "boolean",
        example => 0,
        description => "Enable Pandoc processing of MediaWiki documents (extension=mediawiki)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_org => {
        type => "boolean",
        example => 0,
        description => "Enable Pandoc processing of Emacs org-mode documents (extension=org)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_opml => {
        type => "boolean",
        example => 0,
        description => "Enable Pandoc processing of OPML documents (extension=opml)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_smart => {
        type => "boolean",
        example => 1,
        description => "Use smart quotes, dashes, and ellipses",
        safe => 1,
        rebuild => 1,
    },
    pandoc_obfuscate => {
        type => "boolean",
        example => 1,
        description => "Obfuscate emails",
        safe => 1,
        rebuild => 1,
    },
    pandoc_html5 => {
        type => "boolean",
        example => 0,
        description => "Generate HTML5",
        safe => 1,
        rebuild => 1,
    },
    pandoc_ascii => {
        type => "boolean",
        example => 0,
        description => "Generate ASCII instead of UTF8",
        safe => 1,
        rebuild => 1,
    },
    pandoc_html_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for html",
        safe => 0,
        rebuild => 0,
    },
    pandoc_numsect => {
        type => "boolean",
        example => 0,
        description => "Number sections",
        safe => 1,
        rebuild => 1,
    },
    pandoc_sectdiv => {
        type => "boolean",
        example => 0,
        description => "Attach IDs to section DIVs instead of Headers",
        safe => 1,
        rebuild => 1,
    },
    pandoc_codeclasses => {
        type => "string",
        example => "",
        description => "Classes to use for indented code blocks",
        safe => 1,
        rebuild => 1,
    },
    pandoc_math => {
        type => "string",
        example => "mathjax",
        description => "How to process TeX math (mathjax, katex, mathml, mathjs, latexmathml, asciimathml, mimetex, webtex)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_math_custom_js => {
        type => "string",
        example => "",
        description => "Link to local/custom javascript for math (or to server-side script for mimetex and webtex)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_math_custom_css => {
        type => "string",
        example => "",
        description => "Link to local/custom CSS for math (requires appropriate pandoc_math setting)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_bibliography => {
        type => "string",
        example => "",
        description => "Path to default bibliography file",
        safe => 1,
        rebuild => 1,
    },
    pandoc_csl => {
        type => "string",
        example => "",
        description => "Path to CSL file (for references and bibliography)",
        safe => 1,
        rebuild => 1,
    },
    pandoc_csl_default_lang => {
        type => "string",
        example => "",
        description => "Default language code (RFC 1766) for citations processing",
        safe => 1,
        rebuild => 1,
    },
    pandoc_filters => {
        type => "string",
        example => "",
        description => "A comma-separated list of custom pandoc filters",
        safe => 1,
        rebuild => 1,
    },
    pandoc_latex_template => {
        type => "string",
        example => "",
        description => "Path to pandoc template for LaTeX and normal PDF output",
        safe => 1,
        rebuild => 0,
    },
    pandoc_latex_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for LaTeX (and PDF) generation",
        safe => 0,
        rebuild => 0,
    },
    pandoc_beamer_template => {
        type => "string",
        example => "",
        description => "Path to pandoc template for Beamer PDF output",
        safe => 1,
        rebuild => 0,
    },
    pandoc_beamer_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for Beamer PDF generation",
        safe => 0,
        rebuild => 0,
    },
    pandoc_pdf_export_cleanup => {
        type => "boolean",
        example => "0",
        description => "Whether to clean up LaTeX auxiliary files after PDF generation",
        safe => 0,
        rebuild => 0,
    },
    pandoc_revealjs_template => {
        type => "string",
        example => "",
        description => "Path to pandoc template for Reveal.js slides output",
        safe => 1,
        rebuild => 0,
    },
    pandoc_revealjs_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for Reveal.js slides generation",
        safe => 0,
        rebuild => 0,
    },
    pandoc_docx_template => {
        type => "string",
        example => "",
        description => "Path to pandoc template for MS Word (docx) output",
        safe => 1,
        rebuild => 0,
    },
    pandoc_docx_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for DOCX generation",
        safe => 0,
        rebuild => 0,
    },
    pandoc_odt_template => {
        type => "string",
        example => "",
        description => "Path to pandoc template for OpenDocument (odt) output",
        safe => 1,
        rebuild => 0,
    },
    pandoc_odt_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for ODT generation",
        safe => 0,
        rebuild => 0,
    },
    pandoc_epub_template => {
        type => "string",
        example => "",
        description => "Path to pandoc template for EPUB3 output",
        safe => 1,
        rebuild => 0,
    },
    pandoc_epub_extra_options => {
        type => "internal",
        default => [],
        description => "List of extra pandoc options for EPUB3 generation",
        safe => 0,
        rebuild => 0,
    };
}


sub htmlize ($@) {
    my $format = shift;
    my %params = @_;
    my $page = $params{page};
    my $htmlformat = 'html';

    local(*PANDOC_IN, *JSON_IN, *JSON_OUT, *PANDOC_OUT);
    my @args = ();

    # The default assumes pandoc is in PATH
    my $command = $config{pandoc_command} || "pandoc";

    if ($config{pandoc_smart}) {
        push @args, '--smart';
    }

    if ($config{pandoc_obfuscate}) {
        push @args, '--email-obfuscation=references';
    } else {
        push @args, '--email-obfuscation=none';
    }

    if ($config{pandoc_html5}) {
        $htmlformat = 'html5';
    }

    if ($config{pandoc_ascii}) {
        push @args, '--ascii';
    }

    if ($config{pandoc_numsect}) {
        push @args, '--number-sections';
    }

    if ($config{pandoc_sectdiv}) {
        push @args, '--section-divs';
    }

    if ($config{pandoc_codeclasses} && ($config{pandoc_codeclasses} ne "")) {
        push @args, '--indented-code-classes=' . $config{pandoc_codeclasses};
    }

    # How to process math. Normally either mathjax or katex.
    my %mathconf = map {($_=>"--$_")} qw(
        jsmath mathjax latexmathml asciimathml mathml katex mimetex webtex
    );
    my %with_urls = qw/mimetex 1 webtex 1/;
    my $mathopt = $1 if $config{pandoc_math} && $config{pandoc_math} =~ /(\w+)/;
    my $custom_js = $config{pandoc_math_custom_js} || '';
    # cleanup pandoc-prefixed keys from persistent meta
    if (ref $pagestate{$page}{meta} eq 'HASH') {
        my @delkeys = ();
        foreach my $k (%{ $pagestate{$page}{meta} }) {
            push @delkeys, $k if $k =~ /^pandoc_/;
        }
        delete $pagestate{$page}{meta}{$_} for @delkeys;
    }
    if ($mathopt && $mathconf{$mathopt}) {
        if ($with_urls{$mathopt} && $custom_js) {
            # In these cases, the 'custom js' is a misnomer: actually a server-side script
            push @args, $mathconf{$mathopt} ."=". $custom_js;
        } else {
            push @args, $mathconf{$mathopt};
        }
        $pagestate{$page}{meta}{"pandoc_math"} = $mathopt;
        $pagestate{$page}{meta}{"pandoc_math_$mathopt"} = 1;
        $pagestate{$page}{meta}{"pandoc_math_custom_js"} = $custom_js if $custom_js;
    }
    # Convert to intermediate JSON format so that the title block
    # can be parsed out
    # We must omit the 'bibliography' parameter here, otherwise the list of
    # references will be doubled.
    my $to_json_pid = open2(*JSON_OUT, *PANDOC_OUT, $command,
                    '-f', $format,
                    '-t', 'json',
                    @args, '--normalize',
                    # This is the stupidist fucking "feature" I've ever fucking seen!!!!!
                    # This is not markdown, this is not common mark, this is just fucking stupid bullshit.
                    '--columns=9999');
    error("Unable to open $command") unless $to_json_pid;

    # Workaround for perl bug (#376329)
    require Encode;
    my $content = Encode::encode_utf8($params{content});

    # Protect inline plugin placeholders from being mangled by pandoc:
    $content =~ s{<div class="inline" id="(\d+)"></div>}
                 {::INLINE::PLACEHOLDER::$1::}g;

    print PANDOC_OUT $content;
    close PANDOC_OUT;

    my $json_content = <JSON_OUT>;
    close JSON_OUT;

    waitpid $to_json_pid, 0;

    # Parse the title block out of the JSON and set the meta values
    my $meta = undef;
    my $decoded_json = decode_json($json_content);
    # The representation of the meta block changed in pandoc version 1.18
    if (ref $decoded_json eq 'HASH' && $decoded_json->{'meta'}) {
        $meta = $decoded_json->{'meta'} || {}; # post-1.18 version
    } elsif (ref $decoded_json eq 'ARRAY') {
        $meta = $decoded_json->[0]->{'unMeta'} || {}; # pre-1.18 version
    }
    unless ($meta) {
        warn "WARNING: Unexpected format for meta block. Incompatible version of Pandoc?\n";
    }

    # Get some selected meta attributes, more specifically:
    # (title date bibliography csl subtitle abstract summary description
    #  version lang  locale references author [+ num_authors primary_author]),
    # as well as some configuration options (generate_*, *_extra_options, *_template).

    my @format_keys = grep { $_ ne 'pdf' } keys %extra_formats;
    my %scalar_meta = map { ($_=>undef) } qw(
        title date bibliography csl subtitle abstract summary
        description version lang locale);
    $scalar_meta{$_.'_template'} = undef for @format_keys;
    my %bool_meta = map { ("generate_$_"=>0) } keys %extra_formats;
    my %list_meta = map { ($_=>[]) } qw/author references/;
    $list_meta{$_.'_extra_options'} = [] for @format_keys;
    my $have_bibl = 0;
    foreach my $k (keys %scalar_meta) {
        next unless $meta->{$k};
        $scalar_meta{$k} = compile_string($meta->{$k}->{c});
        # NB! Note that this is potentially risky, since pagestate is sticky, and
        # we only cleanup the pandoc_* values in {meta}.
        $pagestate{$page}{meta}{$k} = $scalar_meta{$k};
        $pagestate{$page}{meta}{"pandoc_$k"} = $pagestate{$page}{meta}{$k};
    }
    foreach my $k (keys %bool_meta) {
        my $gen_all = $meta->{generate_all_formats} || {};
        next unless $meta->{$k} || $gen_all->{c};
        my $val = $meta->{$k} ? $meta->{$k}->{c} : $gen_all->{c};
        # simplifies matters with JSON::(PP::)Boolean objects
        $val = 1 if $val == 1 || $val eq 'true';
        if (ref $val || $val =~ /^\s*(?:off|no|false|0)\s*$/i) {
            $bool_meta{$k} = 0;
        } else {
            $bool_meta{$k} = 1;
            $pagestate{$page}{meta}{"pandoc_$k"} = 1;
        }
    }
    foreach my $k (keys %list_meta) {
        next unless $meta->{$k};
        $list_meta{$k} = unwrap_c($meta->{$k});
        $list_meta{$k} = [$list_meta{$k}] unless ref $list_meta{$k} eq 'ARRAY';
        $have_bibl = 1 if $k eq 'references';
        $pagestate{$page}{meta}{"pandoc_$k"} = $list_meta{$k};
    }
    # Try to add other keys as scalars, with pandoc_ prefix only.
    foreach my $k (keys %$meta) {
        next if exists $scalar_meta{$k} || exists $list_meta{$k};
        eval {
            $pagestate{$page}{meta}{"pandoc_$k"} = compile_string($meta->{$k}->{c});
        };
    }
    my $num_authors = scalar @{ $list_meta{author} };
    $scalar_meta{num_authors} = $num_authors;
    $pagestate{$page}{meta}{num_authors} = $num_authors;
    if ($num_authors) {
        $scalar_meta{primary_author} = $list_meta{author}->[0];
        $pagestate{$page}{meta}{author} = join(', ', @{$list_meta{author}});
        $pagestate{$page}{meta}{pandoc_primary_author} = $scalar_meta{primary_author}
    }

    # The bibliography may be set in a Meta block in the page or in the .setup file.
    # If both are present, the Meta block has precedence.
    for my $bibl ($scalar_meta{bibliography}, $config{pandoc_bibliography}) {
        if ($bibl) {
            $have_bibl = 1;
            $pagestate{$page}{meta}{pandoc_bibliography} = $bibl;
            push @args, '--bibliography='.$bibl;
            last;
        }
    }
    # Similarly for the CSL file...
    for my $cslfile ($scalar_meta{csl}, $config{pandoc_csl}) {
        if ($cslfile) {
            $pagestate{$page}{meta}{pandoc_csl} = $cslfile;
            push @args, '--csl='.$cslfile;
            last;
        }
    }
    # If a default CSL language is specified, add that to args,
    # (unless it is overridden by meta)
    unless ($scalar_meta{lang} || $scalar_meta{locale}) {
        if ($config{pandoc_csl_default_lang}) {
            push @args, "--metadata=lang:".$config{pandoc_csl_default_lang};
        }
    }
    # Turn on the pandoc-citeproc filter if either global bibliography,
    # local bibliography or a 'references' key in Meta is present.
    if ($have_bibl) {
        my $citeproc = $config{pandoc_citeproc} || 'pandoc-citeproc';
        push @args, "--filter=$citeproc";
    }

    # Other pandoc filters. Note that currently there is no way to
    # configure a filter to run before pandoc-citeproc has done its work.
    if ($config{pandoc_filters}) {
        my @filters = split /\s*,\s*/, $config{pandoc_filters};
        s/^["']//g for @filters; # get rid of enclosing quotes
        foreach my $filter (@filters) {
            push @args, "--filter=$filter";
        }
    }

    # html_extra_options my be set in Meta block in the page or in the .setup
    # file.  If both are present, the Meta block has precedence, even if it is
    # an empty list
    my @html_args = @args;
    if (ref $meta->{html_extra_options}{c} eq 'ARRAY') {
      if (ref unwrap_c($meta->{html_extra_options}{c}) eq 'ARRAY') {
        push @html_args, @{unwrap_c($meta->{html_extra_options}{c})};
      } else {
        push @html_args, unwrap_c($meta->{html_extra_options}{c});
      }
    } elsif (ref $config{'pandoc_html_extra_options'} eq 'ARRAY') {
      push @html_args, @{$config{'pandoc_html_extra_options'}};
    }

    my $to_html_pid = open2(*PANDOC_IN, *JSON_IN, $command,
                    '-f', 'json',
                    '-t', $htmlformat,
                    @html_args);
    error("Unable to open $command") unless $to_html_pid;

    $pagestate{$page}{pandoc_extra_formats} = {};
    foreach my $ext (keys %extra_formats) {
        if ($bool_meta{"generate_$ext"}) {
            export_file($page, $ext, $json_content, $command, @args);
        } else {
            remove_exported_file($page, $ext);
        }
    }

    print JSON_IN $json_content;
    close JSON_IN;

    my @html = <PANDOC_IN>;
    close PANDOC_IN;

    waitpid $to_html_pid, 0;

    $content = Encode::decode_utf8(join('', @html));

    # Reinstate placeholders for inline plugin:
    $content =~ s{::INLINE::PLACEHOLDER::(\d+)::}
                 {<div class="inline" id="$1"></div>}g;

    return $content;
}


sub pagetemplate (@) {
    my %params = @_;
    my $page = $params{page};
    my $template = $params{template};
    foreach my $k (keys %{$pagestate{$page}{meta}}) {
        next unless $k =~ /^pandoc_/;
        $template->param($k => $pagestate{$page}{meta}{$k});
    }
}

sub pageactions {
    my %args = @_;
    my $page = $args{page};
    my @links = ();
    return unless $pagestate{$page}{pandoc_extra_formats};
    my @exts = sort {
        $extra_formats{$a}->{order} <=> $extra_formats{$b}->{order}
    } keys %{ $pagestate{$page}{pandoc_extra_formats} };
    foreach my $ext (@exts) {
        my $url = $pagestate{$page}{pandoc_extra_formats}{$ext};
        next unless $url;
        my $label = $extra_formats{$ext}->{label} || $ext;
        push @links, qq[
          <a href="$url"
             class="extra-format-link"
             title="Download $label version of this page"
             target="_blank">$label</a>
        ];
    }
    return @links;
}

sub export_file {
    my ($page, $ext, $json_content, $command, @args) = @_;
    my ($export_path, $export_url) = _export_file_path_and_url($page, $ext);
    my $subdir = $1 if $export_path =~ /(.*)\//;
    my @extra_args = @{ $extra_formats{$ext}->{extra} };
    my $eopt = $ext eq 'pdf' ? 'latex' : $ext;
    # Note that template in meta OVERRIDES template in config,
    # while extra_options in meta are ADDED to extra_options in config.
    my $template = $pagestate{$page}{meta}{"pandoc_".$eopt."_template"}
                   || $config{"pandoc_".$eopt."_template"} || '';
    if ($template) {
        push @extra_args, ($ext =~ /^(docx|odt)$/
                           ? "--reference-$ext=$template"
                           : "--template=$template");
    }
    my $conf_extra = $config{"pandoc_".$eopt."_extra_options"};
    my $conf_extra_custom = $pagestate{$page}{meta}{"pandoc_".$eopt."_extra_options"};
    foreach my $cnf ($conf_extra, $conf_extra_custom) {
        if (ref $cnf eq 'ARRAY' && @$cnf) {
            push @extra_args, @$cnf;
        }
    }
    my $pdf_cleanup = 0;
    if (defined $pagestate{$page}{meta}{"pandoc_pdf_export_cleanup"}) {
        $pdf_cleanup = $pagestate{$page}{meta}{"pandoc_pdf_export_cleanup"};
    } elsif ($config{"pandoc_pdf_export_cleanup"}) {
        $pdf_cleanup = 1;
    }
    # If the user has asked for native LaTeX bibliography handling in the
    # extra_args for this export format (using --biblatex or --natbib),
    # some extra care is needed. Among other things, we need an external
    # tool for PDF generation. In this case, $indirect_pdf will be true.
    my %maybe_non_citeproc = qw/latex 1 pdf 1 beamer 1/;
    my $indirect_pdf = 0;
    if ($maybe_non_citeproc{$ext} && grep { /^(?:--biblatex|--natbib)$/ } @extra_args) {
        $indirect_pdf = 1 unless $ext eq 'latex'; # both for pdf and beamer
        @args = grep { ! /--filter=.*pandoc-citeproc/ } @args;
    }
    eval {
        if ($subdir && !-d $subdir) {
            make_path($subdir) or die "Could not make_path $subdir: $!";
        }
        my $to_format = $extra_formats{$ext}->{format} || $ext;
        my $tmp_export_path = $export_path;
        $tmp_export_path =~ s/\.pdf$/.tex/ if $indirect_pdf;
        open(EXPORT, "|-",
             $command,
             '-f' => 'json',
             '-t' => $to_format,
             '-o' => $tmp_export_path,
             @args, @extra_args) or die "Could not open pipe for $ext: $!";
        print EXPORT $json_content;
        close EXPORT or die "Could not close pipe for $ext: $!";
        if ($indirect_pdf && $tmp_export_path ne $export_path) {
            my @latexmk_args = qw(-quiet -silent);
            if (grep { /xelatex/ } @extra_args) {
                push @latexmk_args, '-xelatex';
            } elsif (grep { /lualatex/ } @extra_args) {
                push @latexmk_args, '-lualatex';
            } else {
                push @latexmk_args, '-pdf';
            }
            chdir $subdir or die "Could not chdir to $subdir: $!";
            my $plain_fn = $1 if $tmp_export_path =~ /([^\/]+)$/;
            $plain_fn =~ s/\.tex//;
            system('latexmk', @latexmk_args, $plain_fn) == 0
                or die "Could not run latexmk for pdf generation ($export_path): $!";
            if ($pdf_cleanup) {
                system('latexmk', '-c', '-quiet', '-silent', $plain_fn) == 0
                    or die "Could not run latexmk for cleanup ($export_path): $!";
                # These files are apparently not cleaned up by latexmk -c.
                foreach ('run.xml', 'bbl') {
                    my $fn = "$subdir/$plain_fn.$_";
                    unlink($fn) if -f $fn;
                }
            }
        }
        $pagestate{$page}{pandoc_extra_formats}{$ext} = $export_url;
    };
    if ($@) {
        warn "EXPORT ERROR FOR $page (format: $ext): $@\n";
    }
}

sub remove_exported_file {
    my ($page, $ext) = @_;
    my ($export_path, $export_url) = _export_file_path_and_url($page, $ext);
    if (-f $export_path) {
        eval { unlink $export_path or die "Could not unlink $export_path: $!" };
        if ($@) {
            warn "WARNING: remove_exported_file; page=$page, ext=$ext: $@\n";
        }
    }
}

sub _export_file_path_and_url {
    my ($page, $ext) = @_;
    # the html file will end up in "$destdir/$page/index.html",
    # while e.g. a pdf will be in "$destdir/$page/$page_minus_dirs.pdf".
    my $extension = $extra_formats{$ext}->{ext} || $ext;
    my $destdir = $config{destdir} || '.';
    my $page_minus_dirs = $1 if $page =~ /([^\/]*)$/;
    $page_minus_dirs ||= 'index';
    my $export_path = "$destdir/$page/$page_minus_dirs.$extension";
    my $export_url = $config{url};
    $export_url .= "/" unless $export_url =~ /\/$/;
    $export_url .= "$page/$page_minus_dirs.$extension";
    return ($export_path, $export_url);
}


## compile_string and unwrap_c are used to make the meta data structures
## easier to work with for perl.

sub compile_string {
    # Partially represents an item from the data structure in meta as a string.
    my @uncompiled = @_;
    return $uncompiled[0] if @uncompiled==1 && !ref($uncompiled[0]);
    @uncompiled = @{$uncompiled[0]} if @uncompiled==1 && ref $uncompiled[0] eq 'ARRAY';
    my $compiled_string = '';
    foreach my $word_or_space (@uncompiled) {
        next unless ref $word_or_space eq 'HASH';
        my $type = $word_or_space->{'t'} || '';
        $compiled_string .= compile_string(@{ $word_or_space->{c} }) if $type eq 'MetaInlines';
        next unless $type eq 'Str' || $type eq 'Space' || $type eq 'MetaString';
        $compiled_string .= $type eq 'Space' ? ' ' : $word_or_space->{c};
    }
    return $compiled_string;
}
sub unwrap_c {
    # Unwrap pandoc's MetaLists, MetaInlines, etc.
    # Finds the deepest-level scalar value for 'c' in the data structure.
    # Lists with one element are replaced with the scalar, lists with more
    # than one element are returned as an arrayref containing scalars.
    my $container = shift;
    if (ref $container eq 'ARRAY' && @$container > 1) {
        if (ref $container->[0] eq 'HASH' && $container->[0]->{t} =~ /^(?:Str|Space)$/) {
            # handles scalar author fields
            return join('', map { compile_string($_) } @$container);
        } else {
            return [map {unwrap_c($_)} @$container];
        }
    } elsif (ref $container eq 'ARRAY' && @$container) {
        return unwrap_c($container->[0]);
    } elsif (ref $container eq 'ARRAY') {
        return;
    } elsif (ref $container eq 'HASH' && $container->{c}) {
        return unwrap_c($container->{c});
    } elsif (ref $container) {
        return;
    } else {
        return $container;
    }
}

1;
