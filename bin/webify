#!/usr/bin/env bash

set -eu

if (( $# == 0 )); then
    cat <<HELP
Usage: $(basename "$0") inputfile outputfile

creates outputfile
HELP
exit 1
fi

haz?() {
    command -v "$1" > /dev/null 2>&1
}

haz? gm && convert_cmd="gm convert"
haz? convert && convert_cmd="convert"

infile="$1"
outfile="$2"

$convert_cmd "$infile" -resize "1280x" -unsharp 2x0.5+0.7+0 -quality 98 -sigmoidal-contrast 3,50% "$outfile"
jpegtran "$outfile" | sponge "$outfile"
