---
layout: post
title: My ZSH (and bash) prompt
image: http://tylercipriani-files.s3.amazonaws.com/blog/junkfoodtheme.png
image_alt: Junkfood theme in git directory
---
In order to become a card-carrying Linux user, I feel like you _need_ to
have spent a truly astounding amount of time fiddling with your
<a href="https://github.com/thcipriani/dotfiles" target="_blank">dotfiles.</a> 

Me? I&#8217;ve gone way beyond the point of diminishing returns. Past the
point where anyone who loves me can even feign interest.
And now I&#8217;m quaffing the sweet nectar of victory, and that victory
nectar _is_ sweet. Oh yes, that&#8217;s right: my insanely customized
prompt is now a part of <a href="http://github.com/robbyrussell/oh-my-zsh" target="_blank">Bobby Russell&#8217;s Oh-My-ZSH</a>

I&#8217;m feeling the pride, joy, and anxiety that can only come from watching
my little utf-8 baby move beyond my home directory and march deep into 
the uncharted home directories of what I can only assume are **BILLIONS** of users.

<h2>All your Oh-My-ZSH are belong to us!
<a href="http://knowyourmeme.com/memes/all-your-base-are-belong-to-us" target="_blank">*</a></h2>

&#8230;or it will when you update your theme. See, cause, oh-my-zsh uses
<a href="http://github.com/robbyrussell/oh-my-zsh/tree/master/themes/" target="_blank">themes</a>
to specify how your prompt looks. You can define what theme you&#8217;d like to use
in your <code>~/.zshrc</code> file. On or around line 8 you&#8217;ll want to
update the line that starts with <code>ZSH_THEME=&#8230;</code> to look like this:

{% highlight bash %}
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="junkfood"
{% endhighlight %}

<h2>Almost, not quite&#8230;</h2>
Like any good dotfile obsessive, I&#8217;ve continued to make changes to this
prompt since I made my pull request to ole Robby! I&#8217;ve modified the prompt
to show the same sort of branch information about SVN repos that it currently 
displays for git repos (e.g., current branch name and local modifications).
That little code chestnut is available over on 
<a href="https://github.com/thcipriani/oh-my-zsh" target="_blank">my fork of the oh-my-zsh build</a>.

<h2>Bash Junkfood theme</h2>
I also have a version of this prompt for Bash that I&#8217;ve made in preperation
for the undoubted overwhelming demand that I expect to begin any time now:

{% highlight bash %}
# An extravagent PS1 http://blog.bigdinosaur.org/easy-ps1-colors/
function prompt {
  # 30m - Black
  # 31m - Red
  # 32m - Green
  # 33m - Yellow
  # 34m - Blue
  # 35m - Purple
  # 36m - Cyan
  # 37m - White
  # 0 - Normal
  # 1 - Bold
  local BLACK="\[\033[0;30m\]"
  local BLACKBOLD="\[\033[1;30m\]"
  local RED="\[\033[0;31m\]"
  local REDBOLD="\[\033[1;31m\]"
  local GREEN="\[\033[0;32m\]"
  local GREENBOLD="\[\033[1;32m\]"
  local YELLOW="\[\033[0;33m\]"
  local YELLOWBOLD="\[\033[1;33m\]"
  local BLUE="\[\033[0;34m\]"
  local BLUEBOLD="\[\033[1;34m\]"
  local PURPLE="\[\033[0;35m\]"
  local PURPLEBOLD="\[\033[1;35m\]"
  local CYAN="\[\033[0;36m\]"
  local CYANBOLD="\[\033[1;36m\]"
  local WHITE="\[\033[0;37m\]"
  local WHITEBOLD="\[\033[1;37m\]"
  export PS1="$WHITEBOLD# $GREEN\u$WHITEBOLD. $BLUE\h$WHITEBOLD. $YELLOW\d$WHITE at $PURPLE\@$WHITEBOLD. $CYAN\w\n  $WHITE"
}
prompt
{% endhighlight %}

<h2>Prompt Inspiration</h2>
Most of the inspiration for moving to ZSH and for creating this _extravagant_ 
(to borrow a phrase) prompt came from a blog post written by Mr. Steve Losh
called, <a href="http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/#oh-my-zsh" target="_blank">
&#8220;My Extravagant ZSH Prompt&#8221;</a>.
