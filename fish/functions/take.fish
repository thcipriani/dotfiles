function take -d 'mkdir and cd'
  set -l dir $argv[1]

  if test -d "$dir"
    set_color red
    echo "Directory exists" >&2
    set_color normal
    return 1
  end

  mkdir "$dir"; and cd "$dir"
end
