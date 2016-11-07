#!/usr/bin/env python2
#
#    Copyright (C) 2016 Tyler Cipriani
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software Foundation,
#    Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
#
# This is a dumb python script for mutt. It reads an email from stdin and tries
# to find a link that starts with the address you pass and opens it
# in a browser.
#
# I have it mapped to ,l in my muttrc via:
#
#     macro pager ,g "|~/.mutt/view-web.py \
#           https://gerrit.wikimedia.org/r/<enter>" \
#           "Open gerrit link in browser"

#     macro pager ,p "|~/.mutt/view-web.py \
#           https://phabricator.wikimedia.org/<enter>" \
#           "Open phab link in browser"
#
import argparse
import os
import sys


ap = argparse.ArgumentParser()

ap.add_argument('--trim',
                help='Stuff to trim from line (after ltrim and rtrim)',
                action='append', default=[])
ap.add_argument('--ltrim', help='Stuff to trim from left of line',
                action='append', default=[])
ap.add_argument('--rtrim', help='Stuff to trim from right of line',
                action='append', default=[])

ap.add_argument('starts_with', help='What the line starts with')

args = ap.parse_args()

for line in sys.stdin.readlines():
    line = line.strip()
    if not line:
        continue

    print line
    print args.starts_with.lower()
    if not line.lower().startswith(args.starts_with.lower()):
        continue

    for left in args.ltrim:
        line = line[len(left):]

    line = line.strip()

    for right in args.rtrim:
        line = line[:len(right) + 1]

    line = line.strip()

    for strip in args.trim:
        line = line.strip(strip)

    line = line.strip()

    print "Calling xdg-open " + line
    os.system('xdg-open "' + line + '"')
    break
