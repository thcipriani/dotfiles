export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# History
shopt -s histappend                  # append to history
# HISTCONTROL=ignoredups:ignorespace # ignore spaces and duplicates
HISTSIZE=10000                       # Up the history commands to remember to 10,000
HISTFILESIZE=20000                   # Up the history to store to 20,000

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
force_color_prompt=yes

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

if [[ "$color_prompt" == "yes" ]]; then
    # * http://blog.bigdinosaur.org/easy-ps1-colors/
    # * https://github.com/eprev/dotfiles/blob/master/includes/prompt.bash
    # * http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
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
else
  BLACK=""
  RED=""
  GREEN=""
  YELLOW=""
  BLUE=""
  MAGENTA=""
  CYAN=""
  WHITE=""

  RESET=""
  BRIGHT=""

  ORANGE=""
  PURPLE=""
  LIME_YELLOW=""
  POWDER_BLUE=""
fi

unset color_prompt force_color_prompt

_parse_git_dirty() {
  if [[ -z "$(git status -s 2> /dev/null)" ]]; then
    printf "${LIME_YELLOW}✔${RESET}"
  else
    printf "${RED}✗${RESET}"
  fi
}

_parse_svn_dirty () {
  if [[ ($(svn st 2> /dev/null) == "") || ($(svn st 2> /dev/null | wc -l) == 1 && $(svn st 2> /dev/null | sed -e 's/\s*\(.\)\s*.*/\1/') == 'S') ]]; then
    printf "${LIME_YELLOW}✔${RESET}"
  else
    printf "${RED}✗${RESET}"
  fi
}

_parse_git_branch() {
  git rev-parse --is-inside-work-tree &>/dev/null || return
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ \1[git]$(_parse_git_dirty)/"
}

_parse_svn_branch() {
  svn info 2> /dev/null || return
  svn info 2> /dev/null | grep -i url | sed -e "s/url: $REPO\/\(.*\)/ \1[svn]$(_parse_svn_dirty)/i"
}

_rprompt() {
  local _time=$1
  (( $_time < 5 )) && return
  local _out
  local days=$(( $_time / 60 / 60 / 24 ))
  local hours=$(( $_time / 60 / 60 % 24 ))
  local minutes=$(( $_time / 60 % 60 ))
  local seconds=$(( $_time % 60 ))
  (( $days > 0 )) && _out="${days}d"
  (( $hours > 0 )) && _out="$_out ${hours}h"
  (( $minutes > 0 )) && _out="$_out ${minutes}m"
  _out="$_out ${seconds}s"
  printf "${RED}$_out${RESET}"
}

prompt() {
    local _exitcode=$?

    local _end_time=$(date +%s)
    local _total_time=$(( _end_time - _start_time ));
    if ((_end_time == _start_time || _start_time == 0)); then
      _total_time=0
    fi

    unset _start_time

    local _color

    if [ $_exitcode -eq 0 ]; then
      color=${CYAN}
    else
      color=${RED}
    fi

    _top_row="\[${LIME_YELLOW}\]\W\[${RESET}\]$(_parse_git_branch)$(_parse_svn_branch)\[${RESET}\]"
    [[ "$SSH_CONNECTION" != '' ]] && \
      _top_row="\[${BRIGHT}${MAGENTA}\]\u\[${RESET}\]@\[${ORANGE}\]\h\[${RESET}\]:${_top_row}"

    # echo 》| hexdump -b
    # 0000000 343 200 213 012                                                
    # 0000004
    _bottom_row="\[${color}\]\343\200\213\[${RESET}\] "

    PS1="${_top_row}\$(_rprompt $_total_time)\n${_bottom_row}"
}

_exec () { :; }

_log_start() {
  [ -n "$COMP_LINE" ] && return  # do nothing if completing
  [[ "$BASH_COMMAND" == "$PROMPT_COMMAND" || "$BASH_COMMAND" =~ "_rprompt" ]] && return # don't cause a preexec for $PROMPT_COMMAND
  _start_time=$(date +%s)
  local this_command=`history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//g"`;
  _exec $this_command;
}

trap '_log_start' DEBUG
PROMPT_COMMAND=prompt
PS2="\[$ORANGE\]→ \[$RESET\]"

if [ -f "$HOME/srv/art/motd/$(hostname -s)_motd" ]; then
  cat "$HOME/srv/art/motd/$(hostname -s)_motd"
fi

for file in ~/.{bash_prompt,exports,aliases,functions}; do
    [ -r "$file" ] && . "$file"
done
unset file
