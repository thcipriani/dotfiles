#!/usr/bin/env bash

prepend_to_path() {
  local dir="$1"

  if [[ -d "${dir}" ]]; then
    if [[ ":$PATH:" != *"${dir}"* ]]; then
      export PATH="${dir}:${PATH}"
    fi
  fi
}

has?() {
    command -v "$1" > /dev/null 2>&1
}

# Make sure "$HOME/bin" is in path
prepend_to_path "$HOME/bin"

# Desktop setup {{{
wallpaper="$HOME/.wallpaper"

# XRandR—{{{ Fix my damn monitor at home. Damnit.
if [ -x "$HOME/.monitorfix" ]; then
    "$HOME/.monitorfix"
fi
# }}}

# Local Hacks {{{
XRDB_OPTIONS=''
[ -r "$HOME/.hacksrc" ] && . "$HOME/.hacksrc"
# }}}

# Fonts {{{
# terminess
test -d "$HOME/.fonts/terminess" && xset +fp "$HOME/.fonts/terminess"

#unifont
test -d "$HOME/.fonts/unifont" && xset +fp "$HOME/.fonts/unifont"
# }}}

# Load X Configuration
test -f "$HOME/.Xresources" && xrdb $XRDB_OPTIONS -merge "$HOME/.Xresources"

# trayer {{{
# No more trayer
# has? starttrayer && starttrayer
# }}}

# background color {{{
if has? feh && [ -d "$wallpaper" ] && has? wallpaper; then
    wallpaper &
else
    xsetroot -solid midnightblue
fi
# }}}
# }}}

# Keyboard stuffs {{{
# ---
# * use right-alt as Alt-Gr key ($ → £)
# * use right ctrl key as compose (ctrl 1 2 → ½)
# * use ctrl+alt+bksp to restart X
# * Capslock → Ctrl
# * Alt-Gr + Space = nbsp/Alt-Gr + Shift + Space = shy nbsp
setxkbmap \
  -layout us \
  -variant altgr-intl \
  -option compose:rctrl \
  -option terminate:ctrl_alt_bksp \
  -option ctrl:nocaps \
  -option nbsp:level3n

if has? xset; then
  # Stop beeping at me (A.K.A turn of PC speaker)
  # https://www.debian-administration.org/article/110/Removing_annoying_console_beeps
  xset b off
  xset r rate 330 60 # Set keyboard repeat rate
fi
has? xcape && xcape -e 'Control_L=Escape' # https://github.com/alols/xcape

# Clipboard stuffs (requires xvkbd)
has? xbindkeys && xbindkeys
# http://mutelight.org/subtleties-of-the-x-clipboard
if has? autocutsel; then
  autocutsel -fork &
  autocutsel -selection PRIMARY -fork &
fi
# }}}

# Disable automute {{{
# https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting
# └─ Lennart Poettering ಠ_ಠ
has? amixer && amixer -c 0 sset "Auto-Mute Mode" Disabled
# }}}

# Cursor/Touch stuff {{{
has? xsetroot && xsetroot -cursor_name  left_ptr

if has? xsetwacom; then
    _touch_id=$(xsetwacom list devices | awk '/Finger/ {print $7}')
    if [ -n $_touch_id ]; then
      xsetwacom set $_touch_id TapTime 25
      xsetwacom set $_touch_id Mode "Absolute"
      xsetwacom set $_touch_id Suppress 1
      xsetwacom set $_touch_id RawSample 20
      xsetwacom set $_touch_id ScrollDistance 40
    fi
fi
# }}}

# Make temp dir {{{
mkdir "/tmp/$USER"
ln -s "/tmp/$USER" "$HOME/tmp"
# }}}

# Fire up apps {{{
has? nm-applet && nm-applet --sm-disable &
has? gnome-power-manager && gnome-power-manager &
# test -x "$HOME/.dropbox-dist/dropboxd" && "$HOME/.dropbox-dist/dropboxd" &
test -x "/usr/bin/gnome-sound-applet" && "/usr/bin/gnome-sound-applet" &
test -x "/usr/bin/dunst" && "/usr/bin/dunst" &
has? doing && nohup doing > /dev/null 2>&1 &
has? xflux && xflux -z 80501
has? volnoti && volnoti
# has? filesync && filesync daemon > /dev/null 2>&1 &
has? batterytrack && nohup batterytrack > /dev/null 2>&1 &
has? syndaemon && syndaemon -i 1 -t -d
has? xautolock && xautolock -time 10 -locker "$HOME/bin/lock" &
LOCK_PID=$!
printf "%d\n" > "$HOME/.cache/autolock.pid"

# Nasa APOD
has? nasapod && nasapod > /dev/null 2>&1 \
  && notify-send "NASA APOD Downloaded" \
  || notify-send "NASA APOD Download Failed" &
# }}}

exec xmonad
