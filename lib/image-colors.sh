#!/usr/bin/env bash

haz?() {
    command -v "$1" >/dev/null 2>&1
}

haz? convert || { echo "ImageMagick `convert` not found"; exit 1; }

image="$@"
convert "$image" -depth 4 +dither -colors 7 -unique-colors txt:- | awk '$3 ~/#/ {print $3}'

