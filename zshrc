export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Arrows for reverse search
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Gotsta have C-r, baby!
bindkey '^R' history-incremental-search-backward

for file in ~/.{exports,aliases,functions}; do
    [ -r "$file" ] && source "$file"
done
unset file

if [ -f "$HOME/srv/art/motd/$(hostname -s)_motd" ]; then
  cat "$HOME/srv/art/motd/$(hostname -s)_motd"
fi
