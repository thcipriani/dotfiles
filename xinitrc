#!/usr/bin/env bash

background_image="$HOME/Dropbox/AA_Fire.jpg"

# Fix my damn monitor at home. Damnit.
if [ "$(hostname)" = "ryu" ] && [ -x "$HOME/.bin/monitorfix" ]; then
    "$HOME/.bin/laptop-monitorfix"
fi

# Load resources
xrdb -merge .Xresources
 
# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --transparent true --tint 0x191970 --height 15 &
 
# Set the background color
if [ -x "/usr/bin/feh" ] && [ -f "$background_image" ]; then
    feh --bg-scale "$background_image"
else
    xsetroot -solid midnightblue
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
    sleep 10
    "$HOME/.dropbox-dist/dropboxd" &
fi
 
exec xmonad
