function tc -d "Create tmux session, keep track of time for session"
  set -l start_time (date +%s)
  set -g -x TERM "screen-256color"

  if type -f mux >/dev/null ^&1
    mux start "$argv[1]"
  else if type -f tmux >/dev/null ^&1
    tmux new -s "$argv[1]"
  else
    unset _start_time
    set_color red
    printf "[ERR] "
    set_color normal
    printf "Tmux not found\n"
  end

  set -l end_time (date +%s)
  set -l total_time (math $end_time - $start_time)
  if test $end_time -eq $start_time; or test $start_time -eq 0
    set total_time 0
  end

  set -l days (math "$total_time / 60 / 60 / 24")
  set -l hours (math "$total_time / 60 / 60 % 24")
  set -l minutes (math "$total_time / 60 % 60")
  set -l seconds (math "$total_time % 60")
  set_color red
  printf "%sd %sh %sm %ss\n" $days $hours $minutes $seconds
  set_color normal
  set -e -l seconds
  set -e -l minutes
  set -e -l days
  set -e -l hours
end
