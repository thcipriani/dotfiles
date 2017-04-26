#!/bin/bash
display_usage() {
    echo -e "\nUsage:\n$0 source_files_dir output_file.jpg\n" 
    echo -e "Example:\n$0 /tmp /tmp/stars_trail.jpg\n" 
}

if [ $# -ne 2 ]; then
	display_usage
	exit 1
fi

SOURCE_FILES_DIR=$1
OUTPUT_FILE=$2
TMP_FILE="/tmp/$$.jpg"
echo "$TMP_FILE"

if [ ! -d "$SOURCE_FILES_DIR" ]; then
    echo -e "\nThe directory $SOURCE_FILES_DIR does not exist.\n"
    exit 1
fi

rm -f "$OUTPUT_FILE"

for FILE in $SOURCE_FILES_DIR/*.jpg; do
    if [ ! -f "$OUTPUT_FILE" ]; then
        cp "$FILE" "$OUTPUT_FILE"
    else
        echo "Processing $FILE..."
        convert "$FILE" "$OUTPUT_FILE" -compose Lighten_Intensity -composite "$TMP_FILE"
        mv "$TMP_FILE" "$OUTPUT_FILE"
    fi
done

rm "$TMP_FILE"
