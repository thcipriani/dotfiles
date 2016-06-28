#!/usr/bin/env python2
# coding: utf-8
#
#   This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Making a playlist from archive.org's search
# ---
# Archive.org has a massive selection of music. Many individual ogg files
# may exist under a single identifier:
# https://archive.org/details/The_Open_Goldberg_Variations-11823
#
# This script automates creating a list of individual files.
#
# As input, this script takes a search list generated via wget following
# the steps here: https://blog.archive.org/2012/04/26/downloading-in-bulk-using-wget/
#
# Usage is like:
#
# ./archive-org-m3u.py < search.csv > 'The Open Golberg Variations.m3u'
#
# -- OR --
#
# echo 'https://archive.org/download/The_Open_Goldberg_Variations-11823' | \
#        ./archive-org-m3u.py > 'The Open Goldberg Variations.m3u

import sys
import requests
from pyquery import PyQuery as pq


URLS = sys.stdin.read()

playlist = []
for url in URLS.splitlines():
    r = requests.get(url)
    r.raise_for_status()
    page = pq(r.text)
    pres = page('pre a')
    for i in range(0, len(pres)):
        link = pres.eq(i).text()
        if not link.endswith('.ogg'):
            continue
        playlist.append(url + '/' + link)


sys.stdout.write('\n'.join(playlist))
