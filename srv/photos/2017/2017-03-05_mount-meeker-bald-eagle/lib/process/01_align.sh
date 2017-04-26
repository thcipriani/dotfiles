#!/usr/bin/env bash

# In order to average my images, I first align them
align_image_stack \
    -a aligned_ -v -m \
    -g 10 -c 16 -C \
    2017-03-04_mount-meeker-bald-eagle_000{18,19,20}.jpg
