function jump -d "Jump to directory"
  cd "$MARKPATH/$argv[1]" ^ /dev/null; or echo "No such mark: $argv[1]"
end

