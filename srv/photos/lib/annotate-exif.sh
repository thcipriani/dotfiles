#!/usr/bin/env bash

DEBUG=0
DRY_RUN=0
OUTPUT_DIR=
FILES=
TAGS=(
    'FocalLength'
    'ExposureTime'
    'Make'
    'Lens'
    'CreateDate'
    'ISO'
    'Model'
    'FNumber'
)

help() {
    cat<<HELP
USAGE:
    $(basename "$0") [OPTIONS] [--] <infiles>

    Everything after "--" is treated as an input file

OPTIONS:

--help|-h           Show this output and exit
--verbose|-v        Show debug output
--dry-run|-d        Show what commands would be run without running them
--output-dir|-o     Where to put finished photos. By default output
                    photos are put in the directory of input photos

HELP
}

has?() {
    command -v "$1" &> /dev/null
}

say() {
    printf '%b\n' "$@"
}

debug() {
    (( DEBUG > 0 )) && {
        say "$(tput setaf 2)[DEBUG]$(tput sgr0) $@"
    }
}

info() {
    say "$(tput setaf 3)[INFO]$(tput sgr0) $@"
}

die() {
    say "$(tput setaf 1)[ERROR]$(tput sgr0) $@"
    exit 1
}

make_tags() {
    local tags
    for tag in "${TAGS[@]}"; do
        tags="$tags -${tag}"
    done

    printf '%s\n' "$tags"
}

make_exif() {
    local file tags

    file="$1"; shift
    tags="$@"

    exiftool -d '%Y-%m-%d %H:%M:%s' -json $tags "$file"
}

get_tag() {
    local exif key
    exif="$1"
    key="$2"
    printf '%b\n' "$exif" | jq -r ".[0].$key"
}

make_annotation() {
    local annotation exif
    exif="$*"
    IFS='' read -r -d '' annotation <<'ANNOTATION'
Make: %s
Model: %s
Lens: %s
Date Taken: %s
Focal Length: %s
Exposure Time: %s
ISO: %s
F Number: f/%s
ANNOTATION
    printf "$annotation" \
        "$(get_tag "$exif" "Make")" \
        "$(get_tag "$exif" "Model")" \
        "$(get_tag "$exif" "Lens")" \
        "$(get_tag "$exif" "CreateDate")" \
        "$(get_tag "$exif" "FocalLength")" \
        "$(get_tag "$exif" "ExposureTime")" \
        "$(get_tag "$exif" "ISO")" \
        "$(get_tag "$exif" "FNumber")"
}

pathinfo() {
    local infile dir filename ext file
    infile="$1"

    dir="${infile%/*}"
    filename="${infile##*/}"

    ext="${filename##*.}"
    file="${filename%.*}"

    printf '%s %s %s %s\n' "$dir" "$filename" "$file" "$ext"
}

run() {
    local tags exif directory filename basename extension final_filename
    for infile in "$@"; do
        # Get focal length using exiftool
        tags=$(make_tags)
        exif=$(make_exif "$infile" "$tags")
        debug "EXIF data:\n${exif}"

        annotation=$(make_annotation "$exif")
        debug "Annotation:\n$annotation"

        set -- $(pathinfo "$infile")
        debug "\ndirectory: $1\nfilename: $2\nbasename: $3\next: $4"
        directory="$1"
        filename="$2"
        basename="$3"
        extension="$4"

        OUTPUT_DIR="${OUTPUT_DIR:=$directory}"
        OUTPUT_DIR="${OUTPUT_DIR%/}"

        debug "Output directory: $OUTPUT_DIR"

        final_filename="${OUTPUT_DIR}/${basename}.annotated.${extension}"
        info "${infile} -> ${final_filename}"

        if (( DRY_RUN > 0 )); then
            echo convert "$infile" \
                -resize "1280x" \
                -fill white  \
                -undercolor '#00000075' \
                -gravity SouthWest \
                -pointsize 18 \
                -weight bold \
                -annotate +15+15 "${annotation}" "${final_filename}"
        else
            convert "$infile" \
                -resize "1280x" \
                -fill white  \
                -undercolor '#00000075' \
                -gravity SouthWest \
                -pointsize 18 \
                -weight bold \
                -annotate +15+15 "${annotation}" "${final_filename}"
        fi
    done
}

main() {
    while (( $# > 0 )) && [[ "$1" == -* ]]; do
        case "$1" in
            --output-dir|-o)
                shift
                OUTPUT_DIR="$1"
                ;;
            --output-dir=*)
                OUTPUT_DIR="${1#*=}"
                ;;
            --dry-run|-d)
                DRY_RUN=1
                ;;
            --verbose|-v)
                DEBUG=1
                ;;
            --help|-h)
                help
                exit
                ;;
            --)
                shift
                break
                ;;
            *)
                help
                exit 1
                ;;
        esac
        shift
    done

    run "$@"
}

has? convert || die "Command 'convert' not found. Exiting..."
has? exiftool || die "Command 'exiftool' not found. Exiting..."
has? jq || die "Command 'jq' not found. Exiting..."

main "$@"
