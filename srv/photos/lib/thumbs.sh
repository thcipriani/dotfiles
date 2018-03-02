#!/usr/bin/env bash
# Make thumbs

set -e

DEBUG=0
DRY_RUN=0
PUBLIC=0
OUTPUT_DIR="${GIT_PHOTO_THUMB_DIR:-.thumbs}"
TITLE="${GIT_PHOTO_TITLE:-Tyler Cipriani}"
FILES=

declare -A THUMBS
THUMBS=(
    [small]=240
    [medium]=640
    [large]=1024
    [xlarge]=2048
)

declare -A EXIF
EXIF=(
    [FocalLength]='Focal Length'
    [ExposureTime]='Exposure Time'
    [Lens]='Lens'
    [CreateDate]='Date Created'
    [ISO]='ISO'
    [Model]='Model'
    [FNumber]='F Number'
    [Make]='Make'
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
--output-dir|-o     Where to put finished photos. First of
                    \$GIT_PHOTO_THUMB_DIR or pwd otherwise
--public|-p         Upload to s3
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
    } || /bin/true
}

info() {
    say "$(tput setaf 3)[INFO]$(tput sgr0) $@"
}

die() {
    say "$(tput setaf 1)[ERROR]$(tput sgr0) $@"
    exit 1
}

# Runs the function or outputs the function as the case may be
doit() {
    (( DRY_RUN > 0 )) && {
        say "$(tput setaf 2)[DRY_RUN]$(tput sgr0) $*"
    } || {
        debug "$*"
        "$@"
    }
}

# Simple calculator
function calc() {
    local result=""
    result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
    #                       └─ default (when `--mathlib` is used) is 20
    #
    if [[ "$result" == *.* ]]; then
        # improve the output for decimal numbers
        printf "$result\n" |
        sed -e 's/^\./0./'           `# add "0" for cases like ".5"` \
            -e 's/^-\./-0./'         `# add "0" for cases like "-.5"` \
            -e 's/0*$//;s/\.$//'     `# remove trailing zeros` \
            -e 's/\.[[:digit:]]*$//'  # don't return decimal points...here
    else
        printf "$result\n"
    fi
}

# GetExif from file
get_exif() {
    local infile tag
    infile="$1"
    tag="$2"
    exiftool -dateFormat '%Y-%m-%d %H:%M:%S' \
        -json \
        "$infile" | jq -r ".[0].${tag}"
}

# Key in from the m5sum of the file's contents
make_key() {
    local hash infile ext first last
    infile="$1"
    ext="$2"
    hash=$(md5sum < "$infile" | awk '{print $1}')
    first=$(printf '%s\n' "$hash" | cut -c -2)
    last=$(printf '%s\n' "$hash" | cut -c 3-)
    printf '%s/%s\n' "${first}" "${last}"
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

# Print "width height" of image passed in
get_dims() {
    local infile
    infile="$1"
    /usr/bin/exiftool -ImageSize "$infile" | \
        awk '{split($4, x, "x"); print x[1]" "x[2]}'
}

# Get image rotation
get_rotation() {
    local infile
    infile="$1"
    /usr/bin/exiftool -Rotate "$infile"  | awk '{print $4}'
}

upload_to_s3() {
    doit s3cmd --acl-public sync "$OUTPUT_DIR"/ s3://photos.tylercipriani.com/thumbs/
    find "$OUTPUT_DIR" -maxdepth 2 -mindepth 2 -type d | \
        awk '{
            split($1, a, "/")
            print "https://photos.tylercipriani.com/thumbs/"a[2]"/"a[3]"/"
        }'
}

# Make thumbnails for jpg images
jpg_thumb() {
    info "Making thumbs for $infile"
    local orig_ext thumb_size infile outfile width height new_width new_height outpath rotate

    infile="$1"
    outfile="$2"
    orig_ext="$3"

    set -- $(get_dims "$infile")

    width="$1"
    height="$2"

    set -- $(get_rotation "$infile")

    rotate="$1"

    doit mkdir -p "$(dirname "$outfile")"

    for size in "${!THUMBS[@]}"; do
        thumb_size="${THUMBS[$size]}"
        if (( width > height )); then
            set -- $(calc "${thumb_size} * ${height}/${width}")
            new_height="$1"
            new_width="$thumb_size"
        else
            set -- $(calc "${thumb_size} * ${width}/${height}")
            new_width="$1"
            new_height="$thumb_size"
        fi

        # $outfile has a format string for the filename, here I just
        # use the size for the filename so it comes out looking like:
        # ./.thumbs/[KEY]/[SIZE].jpg
        outpath=$(printf "$outfile" "$size" "jpg")

        doit /usr/bin/firejail \
            --profile="$HOME/Pictures/lib/convert.profile" \
            /usr/bin/convert \
            "$infile" \
            -quality 80 \
            -rotate ${rotate:-0} \
            -background white \
            -thumbnail "${new_width}x${new_height}!" \
            +set Thumb::URI \
            -depth 8 \
            -sampling-factor '2x2, 1x1, 1x1' \
            -interlace plane \
            "$outpath"

        doit /usr/bin/exiftool \
            -overwrite_original \
            '-icc_profile<='"$HOME"'/Pictures/lib/tinyrgb.icc' "$outpath"

    done

    # FINALLY, copy over the original
    outpath=$(printf "$outfile" "original" "$orig_ext")
    doit cp "$infile" "$outpath"
}

# Generate a readme for the thumbs directory
# This is the dumbest way to do things I could think of, but I think it'll
# do nicely.
readme() {
    local infile readmefile filename thumbext imgtitle imgdesc imgmeta
    infile="$1"

    if (( DRY_RUN > 0 )); then
        readmefile="/dev/stdout"
    else
        readmefile="$2/index.html"
    fi
    filename="$3"
    ext="$4"

    thumbext="${ext}"

    echo "Ext ${ext}"

    case "${ext}" in
        jpg|jpeg|JPEG|JPG|tif|tiff|TIF|TIFF|nef|NEF)
            thumbext="jpg"
            ;;
        *)
            die "Driver for $ext no implemented :(("
    esac


    imgtitle="$(/usr/bin/exiftool -Title "$infile" | awk -F':' '{$1=""; print $0}')"
    imgdesc="$(/usr/bin/exiftool -Description "$infile" | awk -F':' '{$1=""; print $0}')"

    cat<<README > "$readmefile"
<!doctype html>
<html>
<meta charset="utf-8">
<title>${TITLE} :: $imgtitle</title>
<style>
body {
    margin: 64px auto;
    width: 80%;
    max-width: ${THUMBS[xlarge]}px;
    color: #333;
    font: 1em/1.5em sans-serif;
    text-align: center;
}
header { text-align: left; }
img { max-width: 100%; }
table {
    margin: 0 auto;
    border-collapse: collapse;
    border-spacing: 0;
}
td {
    border: 1px solid gainsboro;
    cell-padding: 0.25em;
}
</style>
<body>
<header>
<h1><a href="https://tylercipriani.com">tylercipriani.com</a></h1>
</header>
<figure>
<img src="large.jpg">
<figcaption>
<h1>$imgtitle</h1>
<p>$imgdesc</p>
</figcaption>
</figure>
<p>
<a href="small.${thumbext}">Small</a> |
<a href="medium.${thumbext}">Medium</a> |
<a href="large.${thumbext}">Large</a> |
<a href="original.${ext}">Original</a>
</p>
<table>
<tr>
<th>EXIF info</th>
</tr>
README

    for metadata in "${!EXIF[@]}"; do
        imgmeta="$(get_exif "$infile" "$metadata")"
        cat<<README >> "$readmefile"
            <tr>
                <td>${EXIF[$metadata]}</td>
                <td>$imgmeta</td>
            </tr>
README
    done

    cat<<README >> "$readmefile"
        </table>
        </body>
        </html>
README
}

run() {
    local dir filename file ext outfile key readme

    for infile in "$@"; do
        infile=$(readlink -f "$infile")
        set -- $(pathinfo "$infile")
        dir="$1"
        filename="$2"
        file="$3"
        ext="$4"

        key=$(make_key "$infile" "$ext")

        outfile="${OUTPUT_DIR}/${key}/%s.%s"

        case "${ext}" in
            jpg|jpeg|JPEG|JPG|tif|tiff|TIF|TIFF)
                jpg_thumb "$infile" "${outfile}" "$ext"
                ;;
            *)
                die "Driver for $ext no implemented :(("
        esac
    done

    readme "$infile" "${OUTPUT_DIR}/${key}" "$filename" "$ext"

    (( PUBLIC > 0 )) && {
        upload_to_s3
    } || /bin/true
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
            --public|-p)
                PUBLIC=1
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

    OUTPUT_DIR="${OUTPUT_DIR%/}"

    [ -d "$OUTPUT_DIR" ] && {
        die "OUTPUT_DIR $OUTPUT_DIR is already a directory, exiting..."
    }

    info "Making output dir: $OUTPUT_DIR"
    doit mkdir -p "$OUTPUT_DIR"

    run "$@"
}

has? firejail || die "Command 'firejail' not found. Exiting..."
has? convert || die "Command 'convert' not found. Exiting..."
has? exiftool || die "Command 'exiftool' not found. Exiting..."

main "$@"
