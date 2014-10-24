function tc -d "Create tmux session, keep track of time for session"
  set start_time (date +%s)
  # set -g -x TERM "screen-256color"

  sleep 5

  set end_time (date +%s)
  set total_time (math $end_time - $start_time)
  if test $end_time -eq $start_time; or test $start_time -eq 0
    set total_time 0
  end

  if test $total_time -lt 5
    echo "here"
  end

  echo "here"
  set days (math "$total_time / 60 / 60 / 24")
  set hours (math "$total_time / 60 / 60 % 24")
  set minutes (math "$total_time / 60 % 60")
  set seconds (math "$total_time % 60")
  echo $hoursd
end

