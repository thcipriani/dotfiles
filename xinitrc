#!/usr/bin/env bash
background_image="$HOME/Dropbox/AA_Fire.jpg"

has?() {
    command -v "$1" > /dev/null 2>&1
}

# Fix my damn monitor at home. Damnit.
if [ "$(hostname -s)" = "ryu" ] && [ -x "$HOME/bin/laptop-monitorfix" ]; then
    "$HOME/bin/laptop-monitorfix"
fi

if [ "$(hostname -s)" = "zangief" ] && [ -x "$HOME/bin/work-monitorfix" ]; then
    "$HOME/bin/work-monitorfix"
fi

# Load X Configuration
test -f "$HOME/.Xresources" && xrdb -merge "$HOME/.Xresources"

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --tint 0x1d1f21 --transparent true --height 18 \
 --alpha 0 &

# Set the background color
if has? feh && [ -f "$background_image" ]; then
    feh --bg-scale "$background_image"
else
    xsetroot -solid midnightblue
fi

# Keyboard stuffs
setxkbmap -model pc105 -layout us -option compose:ralt -option terminate:ctrl_alt_bksp -option ctrl:nocaps
if has? xset; then
    xset -b            # Stop beeping at me
    xset r rate 330 60 # Set keyboard repeat rate
fi
has? xcape && xcape -e 'Control_L=Escape' # https://github.com/alols/xcape
has? xbindkeys && xbindkeys

# Cursor stuff
has? xsetroot && xsetroot -cursor_name  left_ptr

# Fire up apps
has? nm-applet && nm-applet --sm-disable &
has? gnome-power-manager && gnome-power-manager &
test -x "$HOME/.dropbox-dist/dropboxd" && "$HOME/.dropbox-dist/dropboxd" &
test -x "/usr/bin/gnome-sound-applet" && "/usr/bin/gnome-sound-applet" &
test -x "/usr/bin/dunst" && "/usr/bin/dunst" &
has? doing && nohup doing & > /dev/null 2>&1
has? xflux && xflux -z 80501

exec xmonad
