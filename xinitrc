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

# Load resources
xrdb -merge .Xresources
 
# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --tint 0x073642 --transparent true --height 18 \
 --alpha 0 &
 
# Set the background color
if has? feh && [ -f "$background_image" ]; then
    feh --bg-scale "$background_image"
else
    xsetroot -solid midnightblue
fi

# Remap control key
#if [ -f "$HOME/.xmodmap" ]; then
#  xmodmap "$HOME/.xmodmap"
#fi
setxbmap -model pc105 -layout us -option compose:ralt -option terminate:ctrl_alt_bksp -option ctrl:nocaps
 
# Fire up apps
if has? nm-applet; then
    nm-applet --sm-disable &
fi
 
if has? gnome-power-manager; then
    sleep 3
    gnome-power-manager &
fi

if [ -x "$HOME/.dropbox-dist/dropboxd" ]; then
    "$HOME/.dropbox-dist/dropboxd" &
fi

if [ -x "/usr/bin/gnome-sound-applet" ]; then
    "/usr/bin/gnome-sound-applet" &
fi

if [ -x "/usr/bin/dunst" ]; then
    "/usr/bin/dunst" &
fi

if has? xset; then
    xset -b            # Stop beeping at me
    xset r rate 330 60 # Set keyboard repeat rate
fi

if has? doing; then
    nohup doing & > /dev/null 2>&1
fi

# I'm going to say that this doesn't work at all. It's running...but...?
if has? xflux; then
    xflux -z 80501
fi

# https://github.com/alols/xcape
if has? xcape; then
    xcape -e 'Control_L=Escape'
fi

exec xmonad
