#!/usr/bin/env bash

# ffmpeg -i "$1" -c:v libx264 -pix_fmt yuv420p -preset slow -threads 0 -b:v 825k -strict -2 -c:a aac -b:a 96k -pass 1 -f mp4 -y /dev/null
# ffmpeg -i "$1" -c:v libx264 -pix_fmt yuv420p -preset slow -threads 0 -b:v 825k -strict -2 -c:a aac -b:a 96k -pass 2 out.mp4
ffmpeg -i "$1" -c:v libx264 -pix_fmt yuv420p -preset slow -threads 0 -b:v 825k -strict -2 -c:a aac -b:a 96k -s 640x360 -pass 1 -f mp4 -y /dev/null
ffmpeg -i "$1" -c:v libx264 -pix_fmt yuv420p -preset slow -threads 0 -b:v 825k -strict -2 -c:a aac -b:a 96k -s 640x360 -pass 2 out.mp4
ffmpeg -i "$1" -c:v libtheora -qscale:v 7 -codec:a libvorbis -qscale:a 3 -s 640x360 out.ogv
