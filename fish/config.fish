# Environment {{{
set -g -x fish_greeting ''
set -g -x EDITOR vim
set -g -x MARKPATH "$HOME/.marks"
# }}}

# Completions {{{
function jesus_fucking_christ_bind_the_fucking_keys_fish
    bind \cn accept-autosuggestion
    bind \cw backward-kill-word
end

function fish_user_keybindings
    jesus_fucking_christ_bind_the_fucking_keys_fish
end

function fish_user_key_bindings
    jesus_fucking_christ_bind_the_fucking_keys_fish
end
# }}}

# aliases {{{
alias gst "git status"
alias gc  "git commit -v"
alias ga  "git add"
alias gp  "git push"
alias gl  "git up"
alias gd  "git diff"
alias rm  "rm -i"
alias cp  "cp -i"
alias mv  "mv -i"
alias :q  exit
alias ll  "ls -AlFh"
alias l   "ls -AlFh --group-directories-first"
alias ef  "$EDITOR $HOME/.config/fish/config.fish"
alias tls "tmux list-sessions"
alias ta  "tmux attach-session -t"
# }}}

# funcitons {{{
function sosh -d "Source fish shell"
  . "$HOME/.config/fish/config.fish"
end

function jump -d "Jump to directory"
  cd "$MARKPATH/$argv[1]" ^ /dev/null; or echo "No such mark: $argv[1]"
end
# }}}

# PATH {{{
function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH "$argv[1]" $PATH
        end
    end
end

prepend_to_path "/usr/games"
prepend_to_path "/usr/local/games"
prepend_to_path "/bin"
prepend_to_path "/sbin"
prepend_to_path "/usr/bin"
prepend_to_path "/usr/sbin"
prepend_to_path "/usr/local/bin"
prepend_to_path "/usr/local/sbin"
prepend_to_path "/usr/local/plan9"
prepend_to_path "/usr/local/go/bin"
prepend_to_path "$HOME/.rbenv/bin"
prepend_to_path "$HOME/.cabal/bin"
prepend_to_path "$HOME/bin"
# }}}