alias gst "git status"
alias gc  "git commit -v"
alias ga  "git add"
alias gp  "git push"
alias gl  "git up"
alias gd  "git diff"
alias rm  "rm -i"
# alias cp  "cp -i"
alias mv  "mv -i"
alias :q  exit

# I am become function
# alias ll  "ls -AlFh"
# alias l   "ls -AlFh --group-directories-first"

alias ef  "$EDITOR $HOME/.config/fish/config.fish"
alias tls "tmux list-sessions"
alias ta  "tmux attach-session -t"

alias less='less -FirSX'
alias ag='ag --pager "\less -FirSX"'
alias ls='ls --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Colored up cat!
# Easy Insall & Pygments: sudo apt-get install python-pip && sudo pip install Pygments
alias c='pygmentize -O style=monokai -f console256 -g'

# Collect my vim usage for _something_…
# http://www.patrick-wied.at/projects/heatmap-keyboard/
alias vim="vim -w ~/.vim_keystrokes"

alias extip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print \$1'"

alias vpad "vim +set\ buftype=nofile +startinsert"
