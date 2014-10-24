set -g -x fish_greeting ''

# {{{ PATH
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