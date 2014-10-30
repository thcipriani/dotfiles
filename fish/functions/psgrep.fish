function psgrep
  ps axuf | grep -v grep | grep "$argv" -i --color=auto
end
