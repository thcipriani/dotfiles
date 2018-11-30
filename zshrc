export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export BROWSER="firefox"

# History
setopt histappend                 # append to history
# HISTCONTROL=ignoredups:ignorespace # ignore spaces and duplicates
HISTSIZE=10000                       # Up the history commands to remember to 10,000
HISTFILESIZE=20000                   # Up the history to store to 20,000
setopt HISTAPPEND HIST_VERIFY SHARE_HISTORY INC_APPEND_HISTORY EXTENDED_HISTORY

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

# modify the prompt to contain git branch name if applicable
git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null)
  if [[ -n $ref ]]; then
    echo " %{$fg_bold[green]%}${ref#refs/heads/}%{$reset_color%}"
  fi
}
setopt promptsubst
export PS1='${SSH_CONNECTION+"%{$fg_bold[green]%}%n@%m:"}%{$fg_bold[blue]%}%c%{$reset_color%}$(git_prompt_info) %# '

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
