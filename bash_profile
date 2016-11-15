[ ! "$TERM" = 'dumb' ] && [ -n "$PS1" ] && . "$HOME/.bashrc"

if [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
    . "$HOME/.rvm/scripts/rvm"
fi
