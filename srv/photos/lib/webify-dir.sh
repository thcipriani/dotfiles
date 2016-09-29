#!/usr/bin/env bash

set -euo pipefail

if (( $# == 0 )); then
    cat <<HELP
Usage: $(basename $0) <in-dir> <out-dir>

converts all jpg files in in-dir to jpgs in out-dir
HELP
exit 1
fi

indir="$1"
outdir="$2"

for infile in "${indir}"/*.JPG; do
    basefile=$(basename "${infile}")
    rotate=$(exiftool "${infile}"  | awk '/Rotate/ {print $4}')
    printf '... Convertng %s to %s\n' "$infile" "${outdir}/${basefile}"
    convert "$infile" -rotate ${rotate:-0} -resize "1280x" -unsharp 2x0.5+0.7+0 -quality 98 -sigmoidal-contrast 3,50% "${outdir}/${basefile}.jpg"
    jpegtran -optimize "${outdir}/${basefile}.jpg" > "${outdir}/${basefile}.optim.jpg"
done
