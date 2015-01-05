function cp -d "show me progress on cp"
  function prompt -d "prompt cmd"
    set_color red
    echo -n "Destination file exists.  Overwrite?"
    set_color normal
    echo -n " [y/N] > "
  end

  set -l src $argv[1]
  set -l dst $argv[2]

  if test -f $dst
    read -l -p prompt overwrite

    switch $overwrite
      case 'y*'
      case 'Y*'
        echo "Okie dokie"
      case '*'
        echo "Canceled"
        return
    end
  end

  if type pv > /dev/null ^&1
    pv $src > $dst
  else
    rsync -WavPh $src $dst
  end
end
