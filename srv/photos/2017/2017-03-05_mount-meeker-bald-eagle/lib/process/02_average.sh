#!/usr/bin/env bash

# Now I'll use g'mic to average these images
gmic -average_files aligned\*.tif -o averaged.jpg,ushort
