#!/usr/bin/env bash

# Now I'll use g'mic to average these images
gmic -median_files "$@" -o averaged.tif,ushort
