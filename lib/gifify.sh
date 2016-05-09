#!/usr/bin/env bash
#
# Gifify Focal Length
# ----
# 1. Grab focal length info
# 2. Convert input files to 1280 width jpg
# 3. Annotate jpg with focal length info
# 4. Compile jpg images into gif
# 5. Cleanup jpg images
#
# Example usage:
#
# ./gifify-focal-length.sh *.nef

declare -a files
for file in "$@"; do
    printf "[INFO] Converting to jpeg: %s\n" "$file"

    # Get focal length using exiftool
    focal_len=$(exiftool "$file" | grep -E '^Focal Length *:' | grep -v '(')

    # Convert to annotated jpg
    convert "$file" -resize "1280x" -fill white  -undercolor '#00000080' \
        -gravity South -annotate +0+5 " ${focal_len} " "${file}.jpg"

    # Add to file array
    files+=("${file}.jpg")
done

printf "[INFO] Compiling gif at: %s\n" "$(pwd)/out.gif"
convert -delay 100 -loop 0 "${files[@]}" out.gif

for file in "${files[@]}"; do
    printf "[INFO] Cleaning jpeg: %s\n" "$file"
    rm "$file"
done
