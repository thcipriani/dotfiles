#!/usr/bin/env bash

background_image="$HOME/Dropbox/AA_Fire.jpg"

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
if [ -x "/usr/bin/feh" ] && [ -f "$background_image" ]; then
    feh --bg-scale "$background_image"
else
    xsetroot -solid midnightblue
fi

# Remap control key
if [ -f "$HOME/.xmodmap" ]; then
  xmodmap "$HOME/.xmodmap"
fi
 
# Fire up apps
if [ -x /usr/bin/nm-applet ] ; then
    nm-applet --sm-disable &
fi
 
if [ -x /usr/bin/gnome-power-manager ] ; then
    sleep 3
    gnome-power-manager &
fi

if [ -x "$HOME/.dropbox-dist/dropboxd" ]; then
    sleep 3
    "$HOME/.dropbox-dist/dropboxd" &
fi

if [ -x "/usr/bin/gnome-sound-applet" ]; then
    sleep 3
    "/usr/bin/gnome-sound-applet" &
fi

if [ -x "/usr/bin/xset" ]; then
    sleep 3
    xset -b
fi

if command -v xflux > /dev/null 2>&1; then
  xflux -z 80501
fi

exec xmonad
