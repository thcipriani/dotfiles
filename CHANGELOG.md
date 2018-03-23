# Version 1.1.0

## Random version tag

I Know. I know. I haven't updated this changlog in a good long while. But now
I want to make note of a few changes I made.

## Changes

* URxvt/XResources font size plugin
* trying out [Base16](http://chriskempson.com/projects/base16/) (thanks @caseydentinger)
* As a result of Base16 use, I've simplifed and decrufted a lot of color usage
* I also had to fiddle with weechat a bit

## Future work

Make xmobar and xmonad realize when I'm using a light vs dark colorscheme...

## Weechat fiddling note

I have no idea what I'm doing with Weechat, but here are the commands I used
to ensure that sutff is readable:

    /set weechat.color.chat_highlight 0
    /set weechat.color.chat_highlight_bg 9
    /set buffers.color.hotlist_message_fg 2
    /set buffers.color.hotlist_low_fg 7
    /set buffers.color.hotlist_highlight_bg 4

# Version 1.0

## Overview

This is the first iteration of my DotFiles that I've given a version, or even
a description.

This is a *major* change. I've decided to combine my major personal git repos
into One Giant Repo™.

This repo now includes my photos, my blog, and my dotfiles.

Look. I know. This sounds crazy.

This is the git repo as an intimate personal archive. The foundation of my
personal computing is this dotfiles repo. I also almost always clone my
`~/Picutres` directory on personal machines as well. I don't always clone my
blog, but it's fairly small. I'd also like the wall between my blog and my
photos to be more permeable and this change supports that goal.

This could be a dumb idea. I don't know. I've never tried it – I don't know
anyone who has. I can back out of this if it's a dumb idea...probably.
