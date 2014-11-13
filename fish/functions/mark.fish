function mark -d "Mark a path for jumping"
  set -l path (pwd)
  mkdir -p "$MARKPATH"
  ln -s "$path" "$MARKPATH/$argv[1]"
end