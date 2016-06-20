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
# to find a 'List-Unsubscribe' header with an http-address.
#
# I have it mapped to ,l in my muttrc via:
#
#     macro pager ,l "|~/.mutt/unsub.py<enter>" \
#           "Find unsub header and open in browser"
#
import os
import sys


for line in sys.stdin.readlines():
    line = line.strip()
    if not line:
        continue

    if not line.lower().startswith('list-unsubscribe:'):
        continue

    targets = line.split(':')[1:]
    urls = ':'.join(targets).split(',')
    for url in urls:
        url = url.strip().lstrip('<').rstrip('>')
        if url.startswith('http'):
            print "Calling xdg-open " + url
            os.system('xdg-open "' + url + '"')
        if url.startswith('mailto:'):
            print "Email: " + url[len('mailto:'):]
