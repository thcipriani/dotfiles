# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

unset color_prompt force_color_prompt

for file in ~/.{extra,bash_prompt,exports,aliases,functions}; do
    [ -r "$file" ] && source "$file"
done
unset file

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if [ -f etc/bashrc ]; then
    . etc/bashrc
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.

function parse_git_dirty {
    if [[ $(git status 2> /dev/null | tail -n1) == "nothing to commit (working directory clean)" ]]; then
        echo -e '\033[0;32m✔'
    else
        echo -e '\033[0;31m✗✗✗'
    fi
}

function parse_svn_dirty {
if [[ ($(svn st 2> /dev/null) == "") || ($(svn st 2> /dev/null | wc -l) == 1 && $(svn st 2> /dev/null | sed -e 's/\s*\(.\)\s*.*/\1/') == 'S') ]]; then
        echo -e '\033[0;32m✔'
    else
        echo -e '\033[0;31m✗✗✗'
    fi
}

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/$(echo -e '\033[00m') on $(echo -e '\033[1;37m')\1$(echo -e '\033[00m')[git]$(parse_git_dirty)/"
}

function parse_svn_branch {
    svn info 2> /dev/null | grep -i url | sed -e "s#url: $REPO\/\(.*\)#$(echo -e '\033[00m') on $(echo -e '\033[1;37m')\1$(echo -e '\033[00m')[svn]$(parse_svn_dirty)#"
}

function prompt {
    # An extravagent PS1 http://blog.bigdinosaur.org/easy-ps1-colors/
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
    local NORMAL="\[\033[00m\]"
    # Minimal prompt
    PS1="$WHITEBOLD# $PURPLE\u$NORMAL at $BLUE\h$NORMAL in $GREEN\w$NORMAL\$(parse_git_branch)\$(parse_svn_branch)\n  $NORMAL"
    # Verbose prompt
    # PS1="$WHITEBOLD# $GREEN\u$WHITEBOLD. $BLUE\h$WHITEBOLD. $YELLOW\d$WHITE at $PURPLE\@$WHITEBOLD. $CYAN\w$NORMAL\$(parse_svn_branch)\n  $NORMAL"
}
prompt
