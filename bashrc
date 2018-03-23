export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export BROWSER="firefox"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# I'm a bad speller.
shopt -s autocd cdspell dirspell globstar

# History
shopt -s histappend                  # append to history
HISTCONTROL=ignoredups:ignorespace # ignore spaces and duplicates
HISTSIZE=10000                       # Up the history commands to remember to 10,000
HISTFILESIZE=20000                   # Up the history to store to 20,000
HISTTIMEFORMAT='%F %T%z '

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
    *color*) color_prompt=yes;;
    rxvt-unicode) color_prompt=yes;;
esac

test -r "$HOME/srv/art/motd/$(hostname -s)_motd" \
  && test "$color_prompt" = "yes" \
  && cat "$HOME/srv/art/motd/$(hostname -s)_motd"

for file in ~/.{bash_prompt,exports,aliases,functions}; do
    [ -r "$file" ] && . "$file"
done
unset file

# set iterm2 key profile for option key to +Esc
# if [[ "$(uname -s)" == 'Darwin' ]]; then
#  set -o vi
#  bind -m vi-insert "\C-l":clear-screen
# fi

BASE16_SHELL=$HOME/.config/base16-shell/
[ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
