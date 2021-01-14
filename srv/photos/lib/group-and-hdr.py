#!/usr/bin/env python
"""
Group and HDR
-------------

This script takes all tif files in the current directory, groups them into 3s,
aligns, and averages.
"""

import os
import shutil
import subprocess


tifs = os.walk(os.path.realpath('.')).next()[2]
raw_input('Group and average: ' + repr(tifs) + '?')
for idx, group in enumerate([tifs[i:i+3] for i in range(0, len(tifs), 3)]):
    group_name = 'group' + str(idx)
    os.mkdir(group_name)
    for photo in group:
        shutil.move(photo, os.path.join(group_name, photo))

groups = os.walk(os.path.realpath('.')).next()[1]
raw_input('Enfuse dirs: ' + repr(groups) + '?')
for group in groups:
    subprocess.check_call(
        '/usr/bin/align_image_stack -a aligned_ -v -m -g 10 -c 16 -C *.tif',
        cwd=group, shell=True)
    subprocess.check_call(
        '/usr/bin/enfuse -o avg.tif aligned_*.tif', cwd=group, shell=True)

raw_input('Move averaged file: ' + repr(groups) + '?')
for group in groups:
    fn = group[len('group'):]
    if len(fn) < 2:
        fn = '0' + fn

    fn = fn + '.tif'
    shutil.move(os.path.join(group, 'avg.tif'), fn)
