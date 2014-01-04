export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="junkfood-invader"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# http://superuser.com/questions/306028/tmux-and-zsh-custom-prompt-bug-with-window-name
DISABLE_AUTO_TITLE=true

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(svn)

source $ZSH/oh-my-zsh.sh

# Arrows for reverse search
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Gotsta have C-r, baby!
bindkey '^R' history-incremental-search-backward

for file in ~/.{exports,aliases,functions,extra}; do
    [ -r "$file" ] && source "$file"
done
unset file

cat "$HOME/Art/$(hostname)"
