#!/usr/bin/env bash

for filename in raw/*.nef; do
    dir=$(dirname "$filename")
    filename=$(basename "$filename")

    ext="${filename##*.}"
    file="${filename%.*}"

    # Convert to a jpeg
    convert "raw/$filename" -resize "1280x" -unsharp 2x0.5+0.7+0 -quality 98 -sigmoidal-contrast 3,50% "edit/${file}.jpg"
done
