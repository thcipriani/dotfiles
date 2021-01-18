# Install ufraw to convert NEFs to jpgs
SRC = $(addprefix edit/,$(addsuffix .jpg, $(basename $(notdir $(wildcard raw/*.nef)))))

.PHONY: all

all: edit $(SRC) timelapse upload

timelapse: timelapse.gif

edit:
	mkdir -p edit

edit/%.jpg: raw/%.nef edit
	convert $< -resize "1280x" -unsharp 2x0.5+0.7+0 -quality 92 $@

timelapse.mp4:
	ffmpeg -i 'edit/%*.jpg' -vcodec libx264 -pix_fmt yuv420p -r 24 -q:v 2 timelapse.mp4

timelapse.gif: timelapse.mp4
	ffmpeg -i $< -pix_fmt rgb24 -r 24 -f gif - | gifsicle --optimize=3 > "$@"

upload: timelapse.gif
	s3cmd put --acl-public "$<" s3://tyler.zone/lincoln-log-timelapse2.gif
