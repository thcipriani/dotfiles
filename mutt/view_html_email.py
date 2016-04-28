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
# This is a small program that parses a Maildir, finds any HTML-encoded
# message parts in Multipart-encoded strings, finds any Content-ID uris,
# and builds an html page, using *hopefully* proper encoding, makes
# temporary copies and Content-ID content, and inserts it into the
# html.
#
# Basically, this should enable you to view html emails, or emails with
# inline images inside of mutt without any hassle.
#
# This work is HEAVILY derived from Akkana Peck's script released under
# GPLv2: <https://github.com/akkana/scripts/blob/master/viewhtmlmail>
#
# That script was unworkable for me since:
# (a) It *seemed* to make the assumption that <copy-message> under mutt
#     output messages in the mbox format rather than the maildir format.
# (b) Gmail inline-html images may all be named "image.png" which didn't
#     work in that script (just overwrote the images).
# (c) Had no chartype attached to the html. This script makes a (probably
#     bad) assumption of utf-8...we'll see.
#
# To use it from mutt, put the following lines in your .muttrc:
#
#     macro pager ,v  \
#         "<copy-message>/tmp/mutttmpbox<enter> \
#         <shell-escape>~/bin/viewhtmlmail.py<enter>" \
#         "View HTML in browser"

import os
import mimetypes
import mailbox
import sys
import time
import shutil
import uuid


MAILDIR = '/tmp/mutttmpbox'
TMPDIR = '/tmp/muttmail'
TMPFILE = os.path.join(TMPDIR, 'email.html')
CONTENTS = []
HTML_FMT = ('<html><head><meta charset="%s" />'
            '<title>%s</title><body>%s</body></html>')


def add_inline_images(msg, html):
    """Add any Content-Id-like things to the html.

    Mainly just a find-and-replace thing
    """
    for content in CONTENTS:
        cid = 'cid:%s' % content['id']
        fs = 'file://%s' % content['filename']

        html = html.replace(cid, fs)

    return html


def handle_html(msg, subject, charset=None):
    """Handle html part of alternative message."""
    if not charset:
        charset = 'utf-8'

    html = HTML_FMT % (charset, subject, msg.get_payload(decode=True))
    return add_inline_images(msg, html)


def check_content_id(msg):
    """Check message part for Content-Id key.

    The use of Content-ID in mail messages seems to mostly be related
    to inline images.

    Since this key is case insensitive, have to loop through all
    kinds of keys to get there :(

    The content-id uri seems...pretty loose.
    <https://tools.ietf.org/html/rfc2392>
    """
    for k in msg.keys():
        if k.lower() != 'content-id':
            continue

        content_id = msg[k]

        if content_id.startswith('<'):
            content_id = content_id[1:]

        if content_id.endswith('>'):
            content_id = content_id[:-1]

        ext = mimetypes.guess_extension(msg.get_content_type())
        fn = os.path.join(TMPDIR, '%s%s' % (str(uuid.uuid4()), ext))

        CONTENTS.append({'id': content_id, 'filename': fn})

        with open(fn, 'wb') as f:
            f.write(msg.get_payload(decode=True))


def handle_msg(msg):
    """Handle possible multipart message."""
    charset = msg.get_charset()

    html_part = None

    for part in msg.walk():
        if part.get_content_subtype() == 'html':
            html_part = part

        check_content_id(part)

    return handle_html(html_part, msg['subject'], charset=charset)


def cleanup():
    """Remove any temp files laying around."""
    time.sleep(2)  # Give it a few for the browser to display the email
    shutil.rmtree(MAILDIR)
    shutil.rmtree(TMPDIR)


def setup():
    """Make temp directories."""
    if os.path.exists(TMPDIR):
        shutil.rmtree(TMPDIR)

    os.makedirs(TMPDIR)


def main():
    """Handle flow control and open browser."""
    setup()

    mbox = mailbox.Maildir(MAILDIR, factory=mailbox.MaildirMessage)

    for msg in mbox:
        msg_html = handle_msg(msg)

    if msg_html:
        with open(TMPFILE, 'wb') as fb:
            fb.write(msg_html)
    else:
        print 'No HTML part found :('
        sys.exit(1)

    print "Calling xdg-open file://" + TMPFILE
    os.system("xdg-open file://" + TMPFILE)

if __name__ == '__main__':
    try:
        main()
    finally:
        # Want to make sure that everything is cleaned up so that next
        # run works
        cleanup()
