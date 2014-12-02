# Environment {{{
set -g -x fish_greeting ''

set -g -x EDITOR  "vim"
# set -g -x BROWSER "firefox"
set -g -x BROWSER "google-chrome"

set -g -x MARKPATH "$HOME/.marks"
set -g -x LANGUAGE "en_US.UTF-8"
set -g -x LANG     "en_US.UTF-8"
set -g -x LC_ALL   "en_US.UTF-8"

set -g -x XDG_CONFIG_HOME "$HOME/.config"
set -g -x XDG_CACHE_HOME  "$HOME/.cache"
set -g -x XDG_DATA_HOME   "$HOME/.local/share"
# }}}

# Completions {{{
function jesus_fucking_christ_bind_the_fucking_keys_fish
    bind \co accept-autosuggestion
    bind \cw backward-kill-word
end

function fish_user_keybindings
    jesus_fucking_christ_bind_the_fucking_keys_fish
end

function fish_user_key_bindings
    jesus_fucking_christ_bind_the_fucking_keys_fish
end
# }}}

test -r "$HOME/srv/art/motd/"(hostname -s)"_motd"; \
  and cat "$HOME/srv/art/motd/"(hostname -s)"_motd"

for file in fish_prompt exports aliases
  if test -r "$XDG_CONFIG_HOME/fish/$file.fish"
    . "$XDG_CONFIG_HOME/fish/$file.fish"
  end
end
