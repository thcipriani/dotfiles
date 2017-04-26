#!/usr/bin/env bash

# Align images
# ---
# By default align image stack tries to find 8 control points in a 5x5 grid
# This finds 16 control points in a 10x10 grid.
align_image_stack \
    -a aligned_ -v -m \
    -g 10 -c 16 -C \
    "$@"
