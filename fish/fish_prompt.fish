function parse_git_dirty
  set -l stats (git status -s ^ /dev/null)
  if test -z "$stats"
     printf (set_color green)"✔"(set_color normal)
   else
     printf (set_color red)"✗"(set_color normal)
  end
end

function parse_git_branch
  if git rev-parse --is-inside-work-tree > /dev/null ^ /dev/null
    printf " %s[git]%s" (git rev-parse --abbrev-ref HEAD) (parse_git_dirty)
  else
    printf " "
  end
end


function fish_prompt
  set last_status $status
  # set -l end_time (date +%s)
  # set -l total_time (math $end_time - $start_time)
  # if test $end_time -eq $start_time; or test $start_time -eq 0
  #   set total_time 0
  # end

  # set -e start_time

  # set -l days (math "$total_time / 60 / 60 / 24")
  # set -l hours (math "$total_time / 60 / 60 % 24")
  # set -l minutes (math "$total_time / 60 % 60")
  # set -l seconds (math "$total_time % 60")
  # set_color red
  # printf "%sd %sh %sm %ss\n" $days $hours $minutes $seconds
  # set_color normal
  # set -e -l seconds
  # set -e -l minutes
  # set -e -l days
  # set -e -l hours

  set -l color (set_color blue)

  if test $last_status -gt 0
    set color (set_color red)
  end

  set -l top_row (set_color green)(prompt_pwd)(set_color normal)(parse_git_branch)(set_color normal)

  set -l bottom_row "$color❯ "(set_color normal)

  printf "%s\n%s" $top_row $bottom_row
end
