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

  set -l color (set_color cyan)

  if test $last_status -gt 0
    set color (set_color red)
  end

  set -l top_row (set_color green)(prompt_pwd)(set_color normal)(parse_git_branch)(set_color normal)

  if set -q SSH_CONNECTION
    set top_row (set_color ff00ff)(whoami)(set_color normal)"@"(set_color ff8300)(hostname -s)":$top_row"
  end

  if set -q CMD_DURATION
    set top_row "$top_row"(set_color red)" $CMD_DURATION"(set_color normal)
  end

  set -l bottom_row "$color❯ "(set_color normal)

  printf "%s\n%s" $top_row $bottom_row

end
