#!/usr/bin/perl
# Page inlining and blogging.
package IkiWiki::Plugin::inline;

use warnings;
use strict;
use Encode;
use IkiWiki 3.00;
use URI;

my %knownfeeds;
my %page_numfeeds;
my @inline;
my $nested=0;

sub import {
	hook(type => "getopt", id => "inline", call => \&getopt);
	hook(type => "getsetup", id => "inline", call => \&getsetup);
	hook(type => "checkconfig", id => "inline", call => \&checkconfig);
	hook(type => "sessioncgi", id => "inline", call => \&sessioncgi);
	hook(type => "preprocess", id => "inline", 
		call => \&IkiWiki::preprocess_inline, scan => 1);
	hook(type => "pagetemplate", id => "inline",
		call => \&IkiWiki::pagetemplate_inline);
	hook(type => "format", id => "inline", call => \&format, first => 1);
	# Hook to change to do pinging since it's called late.
	# This ensures each page only pings once and prevents slow
	# pings interrupting page builds.
	hook(type => "rendered", id => "inline", call => \&IkiWiki::pingurl);
}

sub getopt () {
	eval q{use Getopt::Long};
	error($@) if $@;
	Getopt::Long::Configure('pass_through');
	GetOptions(
		"rss!" => \$config{rss},
		"atom!" => \$config{atom},
		"jsonfeed!" => \$config{jsonfeed},
		"allowrss!" => \$config{allowrss},
		"allowatom!" => \$config{allowatom},
		"allowjsonfeed!" => \$config{allowjsonfeed},
		"pingurl=s" => sub {
			push @{$config{pingurl}}, $_[1];
		},      
	);
}

sub getsetup () {
	return
		plugin => {
			safe => 1,
			rebuild => undef,
			section => "core",
		},
		rss => {
			type => "boolean",
			example => 0,
			description => "enable rss feeds by default?",
			safe => 1,
			rebuild => 1,
		},
		atom => {
			type => "boolean",
			example => 0,
			description => "enable atom feeds by default?",
			safe => 1,
			rebuild => 1,
		},
		jsonfeed => {
			type => "boolean",
			example => 0,
			description => "enable jsonfeed feeds by default?",
			safe => 1,
			rebuild => 1,
		},
		allowrss => {
			type => "boolean",
			example => 0,
			description => "allow rss feeds to be used?",
			safe => 1,
			rebuild => 1,
		},
		allowatom => {
			type => "boolean",
			example => 0,
			description => "allow atom feeds to be used?",
			safe => 1,
			rebuild => 1,
		},
		allowjsonfeed => {
			type => "boolean",
			example => 0,
			description => "allow jsonfeed feeds to be used?",
			safe => 1,
			rebuild => 1,
		},
		pingurl => {
			type => "string",
			example => "http://rpc.technorati.com/rpc/ping",
			description => "urls to ping (using XML-RPC) on feed update",
			safe => 1,
			rebuild => 0,
		},
}

sub checkconfig () {
	if (($config{rss} || $config{atom} || $config{jsonfeed}) && ! length $config{url}) {
		error(gettext("Must specify url to wiki with --url when using --rss, --atom, or --jsonfeed"));
	}
	if ($config{rss}) {
		push @{$config{wiki_file_prune_regexps}}, qr/\.rss$/;
	}
	if ($config{atom}) {
		push @{$config{wiki_file_prune_regexps}}, qr/\.atom$/;
	}
	if ($config{jsonfeed}) {
		push @{$config{wiki_file_prune_regexps}}, qr/\.json$/;
	}
	if (! exists $config{pingurl}) {
		$config{pingurl}=[];
	}
}

sub format (@) {
	my %params=@_;

	# Fill in the inline content generated earlier. This is actually an
	# optimisation.
	$params{content}=~s{<div class="inline" id="([^"]+)"></div>}{
		delete @inline[$1,]
	}eg;
	return $params{content};
}

sub sessioncgi ($$) {
	my $q=shift;
	my $session=shift;

	if ($q->param('do') eq 'blog') {
		my $page=titlepage(decode_utf8(scalar $q->param('title')));
		$page=~s/(\/)/"__".ord($1)."__"/eg; # don't create subdirs
		if (! length $page) {
			error(gettext("please enter a page title"));
		}
		# if the page already exists, munge it to be unique
		my $from=$q->param('from');
		my $add="";
		while (exists $IkiWiki::pagecase{lc($from."/".$page.$add)}) {
			$add=1 unless length $add;
			$add++;
		}
		$q->param('page', "/$from/$page$add");
		# now go create the page
		$q->param('do', 'create');
		# make sure the editpage plugin is loaded
		if (IkiWiki->can("cgi_editpage")) {
			IkiWiki::cgi_editpage($q, $session);
		}
		else {
			error(gettext("page editing not allowed"));
		}
		exit;
	}
}

# Back to ikiwiki namespace for the rest, this code is very much
# internal to ikiwiki even though it's separated into a plugin.
package IkiWiki;

my %toping;
my %feedlinks;

sub preprocess_inline (@) {
	my %params=@_;
	
	if (! exists $params{pages} && ! exists $params{pagenames}) {
		error gettext("missing pages parameter");
	}

	if (! defined wantarray) {
		# Running in scan mode: only do the essentials

		if (yesno($params{trail}) && IkiWiki::Plugin::trail->can("preprocess_trailitems")) {
			# default to sorting by age with fallback to title,
			# the same as inline itself, but let the params
			# override that
			IkiWiki::Plugin::trail::preprocess_trailitems(sort => 'age title', %params);
		}

		return;
	}

	if (yesno($params{trail}) && IkiWiki::Plugin::trail->can("preprocess_trailitems")) {
		scalar IkiWiki::Plugin::trail::preprocess_trailitems(sort => 'age title', %params);
	}

	my $raw=yesno($params{raw});
	my $archive=yesno($params{archive});
	my $rss=(($config{rss} || $config{allowrss}) && exists $params{rss}) ? yesno($params{rss}) : $config{rss};
	my $atom=(($config{atom} || $config{allowatom}) && exists $params{atom}) ? yesno($params{atom}) : $config{atom};
	my $jsonfeed=(($config{jsonfeed} || $config{allowjsonfeed}) && exists $params{jsonfeed})
		? yesno($params{jsonfeed})
		: $config{jsonfeed};
	my $quick=exists $params{quick} ? yesno($params{quick}) : 0;
	my $feeds=exists $params{feeds} ? yesno($params{feeds}) : !$quick && ! $raw;
	my $emptyfeeds=exists $params{emptyfeeds} ? yesno($params{emptyfeeds}) : 1;
	my $feedonly=yesno($params{feedonly});

	# Backwards compatibility
	if (defined $params{show} && $params{show} =~ m/^-?\d+$/) {
		$params{limit} = $params{show};
		delete $params{show};
	}
	if (defined $params{feedshow} && $params{feedshow} =~ m/^\d+$/) {
		$params{feedlimit} = $params{feedshow};
		delete $params{feedshow};
	}

	if (! exists $params{limit} && ! $archive) {
		$params{limit}=10;
	}
	if (! exists $params{feedlimit} && exists $params{limit}) {
		$params{feedlimit}=$params{limit};
	}
	my $title;
	if (exists $params{title}) {
		$title = $params{title};
	}
	else {
		$title = $params{page} ne "index" ? pagetitle($params{page}) : $config{wikiname};
	}
	my $desc;
	if (exists $params{description}) {
		$desc = $params{description} 
	}
	else {
		$desc = $config{wikiname};
	}
	my $actions=yesno($params{actions});
	if (exists $params{template}) {
		$params{template}=~s/[^-_a-zA-Z0-9]+//g;
	}
	else {
		$params{template} = $archive ? "archivepage" : "inlinepage";
	}

	my @list;

	if (exists $params{pagenames}) {
		foreach my $p (qw(sort pages)) {
			if (exists $params{$p}) {
				error sprintf(gettext("the %s and %s parameters cannot be used together"),
					"pagenames", $p);
			}
		}

		@list = split ' ', $params{pagenames};

		if (yesno($params{reverse})) {
			@list=reverse(@list);
		}

		foreach my $p (@list) {
			add_depends($params{page}, $p, deptype($quick ? "presence" : "content"));
		}

		@list = grep { exists $pagesources{$_} } @list;
	}
	else {
		my $num=0;
		if ($params{limit}) {
			$num=$params{limit};
		}
		if ($params{feedlimit} && $num < $params{feedlimit} && $num > 0) {
			$num=$params{feedlimit};
		}
		if ($params{skip} && $num) {
			$num+=$params{skip};
		}

		@list = pagespec_match_list($params{page}, $params{pages},
			deptype => deptype($quick ? "presence" : "content"),
			filter => sub { $_[0] eq $params{page} },
			sort => exists $params{sort} ? $params{sort} : "age title",
			reverse => yesno($params{reverse}),
			($num ? (num => $num) : ()),
		);
	}

	if (exists $params{skip}) {
		@list=@list[$params{skip} .. $#list];
	}
	
	my @feedlist;
	if ($feeds) {
		if (exists $params{feedlimit} &&
		    $params{feedlimit} && @list > $params{feedlimit}) {
			@feedlist=@list[0..$params{feedlimit} - 1];
		}
		else {
			@feedlist=@list;
		}
	}
	
	if ($params{limit} && @list > $params{limit}) {
		@list=@list[0..$params{limit} - 1];
	}

	if ($feeds && exists $params{feedpages}) {
		@feedlist = pagespec_match_list(
			$params{page}, "($params{pages}) and ($params{feedpages})",
			deptype => deptype($quick ? "presence" : "content"),
			list => \@feedlist,
		);
	}

	my ($feedbase, $feednum);
	if ($feeds) {
		# Ensure that multiple feeds on a page go to unique files.
		
		# Feedfile can lead to conflicts if usedirs is not enabled,
		# so avoid supporting it in that case.
		delete $params{feedfile} if ! $config{usedirs};
		# Tight limits on legal feedfiles, to avoid security issues
		# and conflicts.
		if (defined $params{feedfile}) {
			if ($params{feedfile} =~ /\// ||
			    $params{feedfile} !~ /$config{wiki_file_regexp}/) {
				error("illegal feedfile");
			}
			$params{feedfile}=possibly_foolish_untaint($params{feedfile});
		}
		$feedbase=targetpage($params{page}, "", $params{feedfile});

		my $feedid=join("\0", $feedbase, map { $_."\0".$params{$_} } sort keys %params);
		if (exists $knownfeeds{$feedid}) {
			$feednum=$knownfeeds{$feedid};
		}
		else {
			if (exists $page_numfeeds{$params{destpage}}{$feedbase}) {
				if ($feeds) {
					$feednum=$knownfeeds{$feedid}=++$page_numfeeds{$params{destpage}}{$feedbase};
				}
			}
			else {
				$feednum=$knownfeeds{$feedid}="";
				if ($feeds) {
					$page_numfeeds{$params{destpage}}{$feedbase}=1;
				}
			}
		}
	}

	my ($rssurl, $atomurl, $jsonfeedurl, $rssdesc, $atomdesc, $jsonfeeddesc, $jsonfeedp);
	if ($feeds) {
		$jsonfeedp = $feedbase . "json" . $feednum;
		if ($rss) {
			$rssurl=abs2rel($feedbase."rss".$feednum, dirname(htmlpage($params{destpage})));
			$rssdesc = sprintf(gettext("%s (RSS feed)"), $desc);
		}
		if ($atom) {
			$atomurl=abs2rel($feedbase."atom".$feednum, dirname(htmlpage($params{destpage})));
			$atomdesc = sprintf(gettext("%s (Atom feed)"), $desc);
		}
		if ($jsonfeed) {
			$jsonfeedurl=abs2rel($jsonfeedp, dirname(htmlpage($params{destpage})));
			$jsonfeeddesc = sprintf(gettext("%s (jsonfeed)"), $desc);
		}
	}

	my $ret="";

	my $postform = (exists $params{rootpage});
	if (exists $params{postform}) {
		$postform = yesno($params{postform});
	}

	if (length $config{cgiurl} && ! $params{preview} && $postform &&
	    IkiWiki->can("cgi_editpage")) {
		# Add a blog post form, with feed buttons.
		my $formtemplate=template_depends("blogpost.tmpl", $params{page}, blind_cache => 1);
		$formtemplate->param(cgiurl => IkiWiki::cgiurl());
		$formtemplate->param(rootpage => rootpage(%params));
		if ($feeds) {
			if ($rss) {
				$formtemplate->param(rssurl => $rssurl);
				$formtemplate->param(rssdesc => $rssdesc);
			}
			if ($atom) {
				$formtemplate->param(atomurl => $atomurl);
				$formtemplate->param(atomdesc => $atomdesc);
			}
			if ($jsonfeed) {
				$formtemplate->param(jsonfeedurl => $jsonfeedurl);
				$formtemplate->param(jsonfeeddesc => $jsonfeeddesc);
			}
		}
		if (exists $params{postformtext}) {
			$formtemplate->param(postformtext =>
				$params{postformtext});
		}
		else {
			$formtemplate->param(postformtext =>
				gettext("Add a new post titled:"));
		}
		if (exists $params{id}) {
			$formtemplate->param(postformid =>
				$params{id});
		}
		$ret.=$formtemplate->output;
	    	
		# The post form includes the feed buttons, so
		# emptyfeeds cannot be hidden.
		$emptyfeeds=1;
	}
	elsif ($feeds && !$params{preview} && ($emptyfeeds || @feedlist)) {
		# Add feed buttons.
		my $linktemplate=template_depends("feedlink.tmpl", $params{page}, blind_cache => 1);
		if ($rss) {
			$linktemplate->param(rssurl => $rssurl);
			$linktemplate->param(rssdesc => $rssdesc);
		}
		if ($atom) {
			$linktemplate->param(atomurl => $atomurl);
			$linktemplate->param(atomdesc => $atomdesc);
		}
		if ($jsonfeed) {
			$linktemplate->param(jsonfeedurl => $jsonfeedurl);
			$linktemplate->param(jsonfeeddesc => $jsonfeeddesc);
		}
		if (exists $params{id}) {
			$linktemplate->param(id => $params{id});
		}
		$ret.=$linktemplate->output;
	}
	
	if (! $feedonly) {
		my $template;
		if (! $raw) {
			# cannot use wiki pages as templates; template not sanitized due to
			# format hook hack
			eval {
				$template=template_depends($params{template}.".tmpl", $params{page},
					blind_cache => 1);
			};
			if ($@) {
				# gettext can clobber $@
				my $error = $@;
				error sprintf(gettext("failed to process template %s"), $params{template}.".tmpl").": $error";
			}
		}
		my $needcontent=$raw || (!($archive && $quick) && $template->query(name => 'content'));
	
		foreach my $page (@list) {
			my $file = $pagesources{$page};
			my $type = pagetype($file);
			if (! $raw) {
				if ($needcontent) {
					# Get the content before populating the
					# template, since getting the content uses
					# the same template if inlines are nested.
					my $content=get_inline_content($page, $params{destpage});
					$template->param(content => $content);
				}
				$template->param(pageurl => urlto($page, $params{destpage}));
				$template->param(inlinepage => $page);
				$template->param(title => pagetitle(basename($page)));
				$template->param(ctime => displaytime($pagectime{$page}, $params{timeformat}, 1));
				$template->param(mtime => displaytime($pagemtime{$page}, $params{timeformat}));
				$template->param(first => 1) if $page eq $list[0];
				$template->param(last => 1) if $page eq $list[$#list];
				$template->param(html5 => $config{html5});
	
				if ($actions) {
					my $file = $pagesources{$page};
					my $type = pagetype($file);
					if ($config{discussion}) {
						if ($page !~ /.*\/\Q$config{discussionpage}\E$/i &&
						    (length $config{cgiurl} ||
						     exists $pagesources{$page."/".lc($config{discussionpage})})) {
							$template->param(have_actions => 1);
							$template->param(discussionlink =>
								htmllink($page,
									$params{destpage},
									$config{discussionpage},
									noimageinline => 1,
									forcesubpage => 1));
						}
					}
					if (length $config{cgiurl} &&
					    defined $type &&
					    IkiWiki->can("cgi_editpage")) {
						$template->param(have_actions => 1);
						$template->param(editurl => cgiurl(do => "edit", page => $page));

					}
				}
	
				run_hooks(pagetemplate => sub {
					shift->(page => $page, destpage => $params{destpage},
						template => $template,);
				});
	
				$ret.=$template->output;
				$template->clear_params;
			}
			else {
				if (defined $type) {
					$ret.="\n".
					      linkify($page, $params{destpage},
					      preprocess($page, $params{destpage},
					      filter($page, $params{destpage},
					      readfile(srcfile($file)))));
				}
				else {
					$ret.="\n".
					      readfile(srcfile($file));
				}
			}
		}
	}
	
	if ($feeds && ($emptyfeeds || @feedlist)) {
		eval q{use HTML::Entities};
		if ($rss) {
			my $rssp=$feedbase."rss".$feednum;
			will_render($params{destpage}, $rssp);
			if (! $params{preview}) {
				writefile($rssp, $config{destdir},
					genfeed("rss",
						$config{url}."/".$rssp, $title, $desc, $params{guid}, $params{page}, @feedlist));
				$toping{$params{destpage}}=1 unless $config{rebuild};
				$feedlinks{$params{destpage}}.=qq{<link rel="alternate" type="application/rss+xml" title="$rssdesc" href="$rssurl" />};
			}
		}
		if ($atom) {
			my $atomp=$feedbase."atom".$feednum;
			will_render($params{destpage}, $atomp);
			if (! $params{preview}) {
				writefile($atomp, $config{destdir},
					genfeed("atom", $config{url}."/".$atomp, $title, $desc, $params{guid}, $params{page}, @feedlist));
				$toping{$params{destpage}}=1 unless $config{rebuild};
				$feedlinks{$params{destpage}}.=qq{<link rel="alternate" type="application/atom+xml" title="$atomdesc" href="$atomurl" />};
			}
		}
		if ($jsonfeed) {
			will_render($params{destpage}, $jsonfeedp);
			if (! $params{preview}) {
				writefile($jsonfeedp, $config{destdir},
					genfeed("jsonfeed", $config{url} . "/" . $jsonfeedp,
						$title, $desc, $params{guid}, $params{page}, @feedlist));
				$toping{$params{destpage}}=1 unless $config{rebuild};
				my $safejsonfeeddesc = HTML::Entities::encode_entities($jsonfeeddesc);
				my $safejsonfeedurl = HTML::Entities::encode_entities($jsonfeedurl);;
				$feedlinks{$params{destpage}}.=qq{<link rel="alternate" type="application/json" title="$safejsonfeeddesc" href="$safejsonfeedurl" />};
			}
		}

	}
	
	clear_inline_content_cache();

	return $ret if $raw || $nested;
	push @inline, $ret;
	return "<div class=\"inline\" id=\"$#inline\"></div>\n\n";
}

sub pagetemplate_inline (@) {
	my %params=@_;
	my $page=$params{page};
	my $template=$params{template};

	$template->param(feedlinks => $feedlinks{$page})
		if exists $feedlinks{$page} && $template->query(name => "feedlinks");
}

{
my %inline_content;
my $cached_destpage="";

sub get_inline_content ($$) {
	my $page=shift;
	my $destpage=shift;
	
	if (exists $inline_content{$page} && $cached_destpage eq $destpage) {
		return $inline_content{$page};
	}

	my $file=$pagesources{$page};
	my $type=pagetype($file);
	my $ret="";
	if (defined $type) {
		$nested++;
		$ret=htmlize($page, $destpage, $type,
		       linkify($page, $destpage,
		       preprocess($page, $destpage,
		       filter($page, $destpage,
		       readfile(srcfile($file))))));
		$nested--;
		if (isinternal($page)) {
			# make inlined text of internal pages searchable
			run_hooks(indexhtml => sub {
				shift->(page => $page, destpage => $destpage,
					content => $ret);
			});
		}
	}
	
	if ($cached_destpage ne $destpage) {
		clear_inline_content_cache();
		$cached_destpage=$destpage;
	}
	return $inline_content{$page}=$ret;
}

sub clear_inline_content_cache () {
	%inline_content=();
}

}

sub date_822 ($) {
	my $time=shift;

	my $lc_time=POSIX::setlocale(&POSIX::LC_TIME);
	POSIX::setlocale(&POSIX::LC_TIME, "C");
	my $ret=POSIX::strftime("%a, %d %b %Y %H:%M:%S %z", localtime($time));
	POSIX::setlocale(&POSIX::LC_TIME, $lc_time);
	return $ret;
}

sub absolute_urls ($$) {
	# needed because rss sucks
	my $html=shift;
	my $baseurl=shift;

	my $url=$baseurl;
	$url=~s/[^\/]+$//;
	my $urltop; # calculated if needed

	my $ret="";

	eval q{use HTML::Parser; use HTML::Tagset};
	die $@ if $@;
	my $p = HTML::Parser->new(api_version => 3);
	$p->handler(default => sub { $ret.=join("", @_) }, "text");
	$p->handler(start => sub {
		my ($tagname, $pos, $text) = @_;
		if (ref $HTML::Tagset::linkElements{$tagname}) {
			while (4 <= @$pos) {
				# use attribute sets from right to left
				# to avoid invalidating the offsets
				# when replacing the values
				my ($k_offset, $k_len, $v_offset, $v_len) =
					splice(@$pos, -4);
				my $attrname = lc(substr($text, $k_offset, $k_len));
				next unless grep { $_ eq $attrname } @{$HTML::Tagset::linkElements{$tagname}};
				next unless $v_offset; # 0 v_offset means no value
				my $v = substr($text, $v_offset, $v_len);
				$v =~ s/^([\'\"])(.*)\1$/$2/;
				eval q{use HTML::Entities};
				my $dv = decode_entities($v);
				if ($dv=~/^#/) {
					$v=$baseurl.$v; # anchor
				}
				elsif ($dv=~/^(?!\w+:)[^\/]/) {
					$v=URI->new_abs($v, $url)->canonical; # relative url
				}
				elsif ($dv=~/^\//) {
					if (! defined $urltop) {
						# what is the non path part of the url?
						my $top_uri = URI->new($url);
						$top_uri->path_query(""); # reset the path
						$urltop = $top_uri->as_string;
					}
					$v=$urltop.$v; # url relative to top of site
				}
				$v =~ s/\"/&quot;/g; # since we quote with ""
				substr($text, $v_offset, $v_len) = qq("$v");
			}
		}
		$ret.=$text;
	}, "tagname, tokenpos, text");
	$p->parse($html);
	$p->eof;

	return $ret;
}

sub genenclosure {
	my $itemtemplate=shift;
	my $url=shift;
	my $file=shift;

	return unless $itemtemplate->query(name => "enclosure");

	my $size=(srcfile_stat($file))[8];
	my $mime="unknown";
	eval q{use File::MimeInfo};
	if (! $@) {
		$mime = mimetype($file);
	}
	$itemtemplate->param(
		enclosure => $url,
		type => $mime,
		length => $size,
	);
}

sub genfeed ($$$$$@) {
	my $feedtype=shift;
	my $feedurl=shift;
        my $feedtitle=shift;
	my $feeddesc=shift;
	my $guid=shift;
	my $page=shift;
	my @pages=@_;
	
	my $url=URI->new(encode_utf8(urlto($page,"",1)));
	
	my $itemtemplate=template_depends($feedtype."item.tmpl", $page, blind_cache => 1);
	my $content="";
	my $lasttime = 0;
	my $count = 0;
	foreach my $p (@pages) {
		$count++;
		my $u=URI->new(encode_utf8(urlto($p, "", 1)));
		my $pcontent = absolute_urls(get_inline_content($p, $page), $url);
		my $fancy_enclosure_seen = 0;

		$itemtemplate->param(
			title => pagetitle(basename($p)),
			url => $u,
			permalink => $u,
			cdate_822 => date_822($pagectime{$p}),
			mdate_822 => date_822($pagemtime{$p}),
			cdate_3339 => date_3339($pagectime{$p}),
			mdate_3339 => date_3339($pagemtime{$p}),
			lastpage => $count == scalar @pages,
		);

		if (exists $pagestate{$p}) {
			if (exists $pagestate{$p}{meta}{guid}) {
				eval q{use HTML::Entities};
				$itemtemplate->param(guid => HTML::Entities::encode_numeric($pagestate{$p}{meta}{guid}));
			}

			if (exists $pagestate{$p}{meta}{updated}) {
				$itemtemplate->param(mdate_822 => date_822($pagestate{$p}{meta}{updated}));
				$itemtemplate->param(mdate_3339 => date_3339($pagestate{$p}{meta}{updated}));
			}

			if (exists $pagestate{$p}{meta}{enclosure}) {
				my $absurl = $pagestate{$p}{meta}{enclosure};
				my $file = $pagestate{$p}{meta}{enclosurefile};
				genenclosure($itemtemplate, $absurl, $file);
				$fancy_enclosure_seen = 1;
			}
		}

		my $file=$pagesources{$p};
		unless ($fancy_enclosure_seen || defined(pagetype($file))) {
			genenclosure($itemtemplate, $u, $file);
			$itemtemplate->param(simplepodcast => 1);
		}

		$itemtemplate->param(content => $pcontent);

		run_hooks(pagetemplate => sub {
			shift->(page => $p, destpage => $page,
				template => $itemtemplate);
		});

		$content.=$itemtemplate->output;
		$itemtemplate->clear_params;

		$lasttime = $pagemtime{$p} if $pagemtime{$p} > $lasttime;
	}

	my $template=template_depends($feedtype."page.tmpl", $page, blind_cache => 1);
	$template->param(
		wants_absolute_urls => 1,
		title => $feedtitle,
		wikiname => $config{wikiname},
		pageurl => $url,
		content => $content,
		feeddesc => $feeddesc,
		guid => $guid,
		feeddate => date_3339($lasttime),
		feeddate_822 => date_822($lasttime),
		feedurl => $feedurl,
	);
	run_hooks(pagetemplate => sub {
		shift->(page => $page, destpage => $page,
			template => $template);
	});
	
	return $template->output;
}

sub pingurl (@) {
	return unless @{$config{pingurl}} && %toping;

	eval q{require RPC::XML::Client};
	if ($@) {
		debug(gettext("RPC::XML::Client not found, not pinging"));
		return;
	}

	# daemonize here so slow pings don't slow down wiki updates
	defined(my $pid = fork) or error("Can't fork: $!");
	return if $pid;
	chdir '/';
	POSIX::setsid() or error("Can't start a new session: $!");
	open STDIN, '/dev/null';
	open STDOUT, '>/dev/null';
	open STDERR, '>&STDOUT' or error("Can't dup stdout: $!");

	# Don't need to keep a lock on the wiki as a daemon.
	IkiWiki::unlockwiki();

	foreach my $page (keys %toping) {
		my $title=pagetitle(basename($page), 0);
		my $url=urlto($page, "", 1);
		foreach my $pingurl (@{$config{pingurl}}) {
			debug("Pinging $pingurl for $page");
			eval {
				my $client = RPC::XML::Client->new($pingurl);
				my $req = RPC::XML::request->new('weblogUpdates.ping',
					$title, $url);
				my $res = $client->send_request($req);
				if (! ref $res) {
					error("Did not receive response to ping");
				}
				my $r=$res->value;
				if (! exists $r->{flerror} || $r->{flerror}) {
					error("Ping rejected: ".(exists $r->{message} ? $r->{message} : "[unknown reason]"));
				}
			};
			if ($@) {
				error "Ping failed: $@";
			}
		}
	}

	exit 0; # daemon done
}


sub rootpage (@) {
	my %params=@_;

	my $rootpage;
	if (exists $params{rootpage}) {
		$rootpage=bestlink($params{page}, $params{rootpage});
		if (!length $rootpage) {
			$rootpage=$params{rootpage};
		}
	}
	else {
		$rootpage=$params{page};
	}
	return $rootpage;
}

1
