_rprompt() {
  local days=$(( $1 / 60 / 60 / 24 ))
  local hours=$(( $1 / 60 / 60 % 24 ))
  local minutes=$(( $1 / 60 % 60 ))
  local seconds=$(( $1 % 60 ))
  (( $days > 0 )) && out="${days}d"
  (( $hours > 0 )) && out="$out ${hours}h"
  (( $minutes > 0 )) && out="$out ${minutes}m"
  out="$out ${seconds}s"
  RPS1="$out"
}

_prompt_precmd() {
  _end_time=$(date +%s)
  _total_time=$(( _end_time - _start_time ));
  if ((_end_time == _start_time || _start_time == 0)); then
    _total_time=0
  fi

  ((_total_time > 5)) && _rprompt "$_total_time"
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
  [[ "$SSH_CONNECTION" != '' ]] && prompt_pure_username='%n@%m '

  # prompt turns red if the previous command didn't exit with 0
  PROMPT='%(?.%F{cyan}.%F{red})❯%f '
}

_prompt_setup "$@"