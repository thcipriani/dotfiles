# Interactive bash? Source bashrc instead why don't ya?
[ -n "$PS1" ] && [ -x "$HOME/.bashrc" ] && . "$HOME/.bashrc"