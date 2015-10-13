#!/usr/bin/env bash

if (( $# == 0 )); then
    cat <<HELP
Usage: $(basename $0) inputfile.ext

creates inputfile.thumb.ext
HELP
exit 1
fi

haz?() {
    command -v "$1" > /dev/null 2>&1
}

haz? gm && convert_cmd="gm convert"
haz? convert && convert_cmd="convert"
test -z "$convert_cmd" && exit 1

infile="$1"

dir=$(dirname "$infile")
filename=$(basename "$infile")

ext="${filename##*.}"
file="${filename%.*}"

# $convert_cmd "$infile" -resize "1280x" -unsharp 2x0.5+0.7+0 -quality 98 -sigmoidal-contrast 3,50% "${dir}/${file}.thumb.${ext}"
# Convert to a jpeg
$convert_cmd "$infile" -resize "1280x" -unsharp 2x0.5+0.7+0 -quality 98 -sigmoidal-contrast 3,50% "${dir}/${file}.thumb.jpg"
