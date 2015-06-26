function g -d "git"
  if count $argv
    git $argv
  else
    git hist3
  end
end
