_rprompt() {
  local _time=$1
  (( $_time < 5 )) && RPS1="" && return
  local _out
  local days=$(( $_time / 60 / 60 / 24 ))
  local hours=$(( $_time / 60 / 60 % 24 ))
  local minutes=$(( $_time / 60 % 60 ))
  local seconds=$(( $_time % 60 ))
  (( $days > 0 )) && _out="${days}d"
  (( $hours > 0 )) && _out="$_out ${hours}h"
  (( $minutes > 0 )) && _out="$_out ${minutes}m"
  _out="$_out ${seconds}s"
  RPS1="${RED}$_out${RESET}"
}

_prompt_precmd() {
  _end_time=$(date +%s)
  _total_time=$(( _end_time - _start_time ));
  if ((_end_time == _start_time || _start_time == 0)); then
    _total_time=0
  fi

  _rprompt "$_total_time"
  unset _start_time
}

_prompt_preexec() {
  _start_time=$(date +%s);
}

_prompt_setup() {
  prompt_opts=(cr subst percent)

  autoload -Uz add-zsh-hook
  autoload -Uz vcs_info

  add-zsh-hook precmd _prompt_precmd
  add-zsh-hook preexec _prompt_preexec

  zstyle ':vcs_info:*' enable git
  zstyle ':vcs_info:git*' formats ' %b'
  zstyle ':vcs_info:git*' actionformats ' %b|%a'

  # show username@host if logged in through SSH
  [[ "$SSH_CONNECTION" != '' ]] && _top_row='%F{magenta}%n%F{reset}@%F{orange}%m%F{reset} '

  # prompt turns red if the previous command didn't exit with 0
  PROMPT="${_top_row}%F{green}%1~%F{reset}
%(?.%F{cyan}.%F{red})❯%f "
}

_prompt_setup "$@"