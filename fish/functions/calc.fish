function calc
  set -l comm "scale=10;$argv"
  #                  └─ default (when `--mathlib` is used) is 20

  set -l result (echo $comm | bc --mathlib | tr -d '\\\n')
  if printf "$result\n" | grep '\.' > /dev/null
      # improve the output for decimal numbers
      printf "$result" |     \
      sed -e 's/^\./0./' \
          -e 's/^-\./-0./' \
          -e 's/0*$//;s/\.$//'
  else
      printf "$result"
  end
  printf "\n"
  set -e result
end
