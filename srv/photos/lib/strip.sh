#!/usr/bin/env bash

if (( $# == 0 )); then
    cat<<HELP
Usage: $(basename $0) file.ext

Creates file.optim.ext
HELP
    exit 1
fi

haz?() {
    command -v "$1" > /dev/null 2>&1
}

haz? jpegtran || exit 1

infile="$1"

dir=$(dirname "$infile")
filename=$(basename "$infile")

ext="${filename##*.}"
file="${filename%.*}"

jpegtran -copy none -optimize -progressive -perfect "$infile" > "${dir}/${file}.optim.${ext}"