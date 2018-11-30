#!/usr/bin/env bash
# This information is taken from: https://commons.wikimedia.org/wiki/Commons:Exif

tag() {
    local img="$1"

    exiftool -m -Artist='Tyler Cipriani' -Author='Tyler Cipriani' \
    -Copyright="This work is licensed under the Creative Commons Attribution ShareAlike 4.0 International License. \
To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/4.0/ or send a \
letter to Creative Commons, 171 Second Street, Suite 300, San Francisco, California, 94105, USA." \
    -XMP-cc:License='http://creativecommons.org/licenses/by-sa/4.0/' "$img"
}

main() {
    for img in "$@"; do
        tag "$img"
    done
}

main "$@"
