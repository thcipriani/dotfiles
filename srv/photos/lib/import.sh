#!/usr/bin/env bash

set -euo pipefail

# 0 == THIS IS NOT A TEST!
SHOOT=
DEST_DIR=
DRY_RUN=0
START_DIR=/mnt/sd-card/DCIM/100ND610
START_FILE_NUM=1
END_FILE_NUM=999999999
EXTENSION=NEF

help() {
    cat<<HELP
Usage: $(basename $0) <--shoot-name|-n name> [--start-file-number|-s n] [--end-file-number|-e n] [--from-dir|-f dir-name] [--extension nef] [--dry-run]

--from-dir|f    default /mnt/sd-card/DCIM/100ND610
HELP
}

say() {
    printf '[...] %s\n' "$@"
}

has?() {
    command -v "$1" &> /dev/null
}

run() {
    local count=0
    local pad_count exif_created new_fn

    (( DRY_RUN > 0 )) && say "Doing dry run"

    say "make dir $DEST_DIR"

    (( DRY_RUN == 0 )) &&  {
        mkdir -p "$DEST_DIR"
    }

    count="$(find "$DEST_DIR" -type f | wc -l)"
    if (( count == 0 )); then
        count=1
    fi

    for file in "${START_DIR}"/*."${EXTENSION}"; do
        set -- "$(printf '%s\n' "$file" | awk \
            -v start="$START_FILE_NUM" \
            -v end="$END_FILE_NUM" \
            '{split($0, a, "."); split(a[1], b, "_"); if (b[2]>=start && b[2]<end) {print $0}}')"

        [ -z "$1" ] && continue

        pad_count=$(printf "%05d\n" $count)
        exif_created=$(exiftool -d '%Y-%m-%d' -printFormat '$CreateDate' "$file")
        new_fn="${DEST_DIR}/${exif_created}_${SHOOT}_${pad_count}.${EXTENSION,,}"

        say "$file -> $new_fn"

        (( DRY_RUN == 0 )) && {
            cp "$file" "$new_fn"
        }

        (( count++ ))
    done
}

main() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --shoot-name|-n)
                shift
                SHOOT="$1"
                ;;
            --shoot-name=*)
                SHOOT="${1#*=}"
                ;;
            --start-file-number|-s)
                shift
                START_FILE_NUM="$1"
                ;;
            --start-file-number=*)
                START_FILE_NUM="${1#*=}"
                ;;
            --end-file-number|-e)
                shift
                END_FILE_NUM="$1"
                ;;
            --end-file-number=*)
                END_FILE_NUM="${1#*=}"
                ;;
            --from-dir|-f)
                shift
                START_DIR="$1"
                ;;
            --from-dir=*)
                START_DIR="${1#*=}"
                ;;
            --extension)
                shift
                EXTENSION="$1"
                ;;
            --extension=*)
                EXTENSION="${1#*=}"
                ;;

            --dry-run|-d)
                DRY_RUN=1
                ;;
            *)
                help
                exit
                ;;
        esac
        shift
    done

    [ -z "$SHOOT" ] && {
        say "--shoot-name must be set"
        exit 2
    }

    DEST_DIR="$HOME/Pictures/$(date +%Y)/$(date --rfc-3339=date)_${SHOOT}/raw"

    run
}

has? exiftool || {
    say "Command exiftool not found. Exiting..."
    exit 1
}

main "$@"
