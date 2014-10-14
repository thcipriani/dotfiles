#!/usr/bin/env bash

has?() {
    command -v "$1" > /dev/null 2>&1
}

# Desktop setup {{{
wallpaper="$HOME/.wallpaper"

# XRandR—{{{ Fix my damn monitor at home. Damnit.
if [ "$(hostname -s)" = "ryu" ] && [ -x "$HOME/bin/laptop-monitorfix" ]; then
    "$HOME/bin/laptop-monitorfix"
fi

if [ "$(hostname -s)" = "zangief" ] && [ -x "$HOME/bin/work-monitorfix" ]; then
    "$HOME/bin/work-monitorfix"
fi
# }}}

# Fonts {{{
# terminess
test -d "$HOME/.fonts/terminess" && xset +fp "$HOME/.fonts/terminess"

#unifont
test -d "$HOME/.fonts/unifont" && xset +fp "$HOME/.fonts/unifont"
# }}}

# Load X Configuration
test -f "$HOME/.Xresources" && xrdb -merge "$HOME/.Xresources"

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
  xset -b            # Stop beeping at me (A.K.A turn of PC speaker)
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

# Cursor/Touch stuff {{{
has? xsetroot && xsetroot -cursor_name  left_ptr

if has? xsetwacom; then
  _touch_id=$(xsetwacom list devices | awk '/Finger/ {print $7}')
  xsetwacom set $_touch_id TapTime 25
  xsetwacom set $_touch_id Mode "Absolute"
  xsetwacom set $_touch_id Suppress 1
  xsetwacom set $_touch_id RawSample 20
  xsetwacom set $_touch_id ScrollDistance 40
fi
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
has? filesync && filesync daemon > /dev/null 2>&1 &
has? batterytrack && nohup batterytrack > /dev/null 2>&1 &

# Nasa APOD
has? nasapod && nasapod > /dev/null 2>&1 \
  && notify-send "NASA APOD Downloaded" \
  || notify-send "NASA APOD Download Failed" &
# }}}

exec xmonad
