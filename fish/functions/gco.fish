function gco -d "git checkout"
  if count $argv
    git checkout $argv
  else
    git branch
  end
end