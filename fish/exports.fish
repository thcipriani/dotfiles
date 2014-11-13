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
prepend_to_path (command ruby -e 'print Gem.user_dir')"/bin"
prepend_to_path "$HOME/.rbenv/bin"
prepend_to_path "$HOME/.cabal/bin"
prepend_to_path "$HOME/bin"
# }}}

set -g -x OPSCODE_USER thcipriani
set -g -x PAGER 'less -FirSX'
set -g -x LESS '-FirSX'

set -x LESS_TERMCAP_mb (printf "\e[01;31m")       # begin blinking
set -x LESS_TERMCAP_md (printf "\e[01;38;5;74m")  # begin bold
set -x LESS_TERMCAP_me (printf "\e[0m")           # end mode
set -x LESS_TERMCAP_se (printf "\e[0m")           # end standout-mode
set -x LESS_TERMCAP_so (printf "\e[38;5;246m")    # begin standout-mode - info box
set -x LESS_TERMCAP_ue (printf "\e[0m")           # end underline
set -x LESS_TERMCAP_us (printf "\e[04;38;5;146m") # begin underline

# Keychain to manage gpg-agent and ssh-agent
if which keychain > /dev/null ^&1
  eval (keychain --eval)
end
