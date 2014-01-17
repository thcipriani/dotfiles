export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

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

for file in ~/.{bash_prompt,exports,aliases,functions}; do
    [ -r "$file" ] && . "$file"
done
unset file

[ -r "$HOME/Dropbox/Code/Bash/dotfile-extras" ] && . "$HOME/Dropbox/Code/Bash/dotfile-extras"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

parse_git_dirty() {
    if [[ $(git status 2> /dev/null | tail -n1) == "nothing to commit (working directory clean)" ]]; then
        echo -e '\033[0;32m✔'
    else
        echo -e '\033[0;31m✗✗✗'
    fi
}

parse_svn_dirty () {
if [[ ($(svn st 2> /dev/null) == "") || ($(svn st 2> /dev/null | wc -l) == 1 && $(svn st 2> /dev/null | sed -e 's/\s*\(.\)\s*.*/\1/') == 'S') ]]; then
        echo -e '\033[0;32m✔'
    else
        echo -e '\033[0;31m✗✗✗'
    fi
}

parse_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1[git]$(parse_git_dirty)/"
}

parse_svn_branch() {
    svn info 2> /dev/null | grep -i url | sed -e "s/url: $REPO\/\(.*\)/\1[svn]$(parse_svn_dirty)/i"
}

prompt() {
    exitcode=$?
    # An extravagent PS1
    #   - http://blog.bigdinosaur.org/easy-ps1-colors/
    #   - https://github.com/eprev/dotfiles/blob/master/includes/prompt.bash
    #   - http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
    #
    # Color   | Escape | Code       | tput-8    | tput-256
    # --------+--------+------------+-----------+----------
    # Black   | 30m    | \033[0;30m | tput 0    | tput 236
    # Red     | 31m    | \033[0;31m | tput 1    | tput 196
    # Green   | 32m    | \033[0;32m | tput 2    | tput 118
    # Yellow  | 33m    | \033[0;33m | tput 3    | tput 226
    # Blue    | 34m    | \033[0;34m | tput 4    | tput 69
    # Magenta | 35m    | \033[0;35m | tput 5    | tput 135
    # Cyan    | 36m    | \033[0;36m | tput 6    | tput 75
    # White   | 37m    | \033[0;37m | tput 7    | tput 254
    # Reset   | 0      | \033[00m   | tput sgr0 | tput sgr0
    # Bold    | 1      | \033[1;xxm | tput bold | tput bold
    #
    # Prompt stolen from:
    #   https://github.com/mathiasbynens/dotfiles/blob/master/.bash_prompt
    if tput setaf 1 &> /dev/null; then
      tput sgr0

      BLACK=$(tput setaf 0)
      RED=$(tput setaf 1)
      GREEN=$(tput setaf 2)
      YELLOW=$(tput setaf 3)
      BLUE=$(tput setaf 4)
      MAGENTA=$(tput setaf 5)
      CYAN=$(tput setaf 6)
      WHITE=$(tput setaf 7)

      BRIGHT=$(tput bold)
      RESET=$(tput sgr0)
      BLINK=$(tput blink)
      REVERSE=$(tput smso)
      UNDERLINE=$(tput smul)

      PURPLE=$(tput setaf 5)
      ORANGE=$(tput setaf 1)
      LIME_YELLOW=$(tput setaf 2)
      POWDER_BLUE=$(tput setaf 4)

      if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
        GREEN=$(tput setaf 190)
        MAGENTA=$(tput setaf 9)
        ORANGE=$(tput setaf 172)
        PURPLE=$(tput setaf 141)
        WHITE=$(tput setaf 254)
        LIME_YELLOW=$(tput setaf 190)
        POWDER_BLUE=$(tput setaf 153)
      fi
    else
      BLACK="\033[0;30m"
      RED="\033[0;31m"
      GREEN="\033[0;32m"
      YELLOW="\033[0;33m"
      BLUE="\033[0;34m"
      MAGENTA="\033[0;35m"
      CYAN="\033[0;36m"
      WHITE="\033[0;37m"

      RESET="\033[00m"
      BRIGHT=""

      ORANGE="\033[1;31m"
      PURPLE="\033[1;35m"
      LIME_YELLOW="\033[1;32m"
      POWDER_BLUE="\033[1;34m"
    fi

    colors=(BLACK \
            RED \
            GREEN \
            YELLOW \
            BLUE \
            MAGENTA \
            CYAN \
            WHITE \
            RESET \
            BOLD \
            ORANGE \
            PURPLE \
            LIME_YELLOW \
            POWDER_BLUE)

    for color in $colors; do
      export $color
    done

    if [ $exitcode -eq 0 ]; then
      color=${WHITE}
    else
      color=${RED}
    fi

    if [ $UID -eq 0 ]; then
      SIGIL="\[${color}\]# \[${RESET}\]"
    else
      SIGIL="\[${color}\]$ \[${RESET}\]"
    fi

    PS1="\[${BRIGHT}${MAGENTA}\]\u \[$WHITE\]at \[$ORANGE\]\h \[$WHITE\]in \[$GREEN\]\w\[$WHITE\]\$([[ -n \$(git branch 2> /dev/null) ]] && echo \" on \")\[$NORMAL\]\$(parse_git_branch)\[$WHITE\]\$([[ -n \$(svn st 2> /dev/null) ]] && echo \" on \")\[$NORMAL\]\$(parse_svn_branch)\[$WHITE\]\n${SIGIL}\[$RESET\]"
}

PROMPT_COMMAND=prompt
PS2="\[$ORANGE\]→ \[$RESET\]"


if [ -f "$HOME/srv/art/motd/$(hostname)_motd" ]; then
  cat "$HOME/srv/art/motd/$(hostname)_motd"
fi
