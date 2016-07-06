#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
  stdgif -- dumb software that shows gifs on the cli.

  The super smart algorithm (that I had nothing to do with) that shows
  amazing ansi renderings of images was ported from Stefan Haustein's
  TerminalImageViewer <https://github.com/stefanhaustein/TerminalImageViewer>

  Requirements: requests <http://docs.python-requests.org>,
                Pillow <https://python-pillow.org/>

  usage: stdgif [-h] [-w WIDTH] [-f] [-d DELAY] [-o OUTPUT] [-s SEPERATOR] img

  positional arguments:
    img                   File to show

  optional arguments:
    -h, --help            show this help message and exit
    -w WIDTH, --width WIDTH
                          Width of file to show
    -f, --forever         Loop forever
    -d DELAY, --delay DELAY
                          The delay between images that make up a gif
    -o OUTPUT, --output OUTPUT
                          Generated bash script path - suitable for sourcing
                          from your .bashrc
    -s SEPERATOR, --seperator SEPERATOR
                          Print the seperator between frames of a gif (this can
                          be useful if piping output into another file or
                          program)

  Copyright (c) 2016 Tyler Cipriani <tyler@tylercipriani.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from __future__ import print_function

import argparse
import math
import os
import sys
import tempfile
import time

import requests

from PIL import Image


FRAMES = {}

BITMAPS = {
    0x00000000: ' ',
    # Block graphics
    0x0000000f: u'\u2581',  # lower 1/8
    0x000000ff: u'\u2582',  # lower 1/4
    0x00000fff: u'\u2583',
    0x0000ffff: u'\u2584',  # lower 1/2
    0x000fffff: u'\u2585',
    0x00ffffff: u'\u2586',  # lower 3/4
    0x0fffffff: u'\u2587',
    0xeeeeeeee: u'\u258a',  # left 3/4
    0xcccccccc: u'\u258c',  # left 1/2
    0x88888888: u'\u258e',  # left 1/4
    0x0000cccc: u'\u2596',  # quadrant lower left
    0x00003333: u'\u2597',  # quadrant lower right
    0xcccc0000: u'\u2598',  # quadrant upper left
    0xcccc3333: u'\u259a',  # diagonal 1/2
    0x33330000: u'\u259d',  # quadrant upper right

    # Line drawing subset: no double lines, no complex light lines
    # Simple light lines duplicated because there is no center pixel int
    # the 4x8 matrix
    0x000ff000: u'\u2501',  # Heavy horizontal
    0x66666666: u'\u2503',  # Heavy vertical

    0x00077666: u'\u250f',  # Heavy down and right
    0x000ee666: u'\u2513',  # Heavy down and left
    0x66677000: u'\u2517',  # Heavy up and right
    0x666ee000: u'\u251b',  # Heavy up and left

    0x66677666: u'\u2523',  # Heavy vertical and right
    0x666ee666: u'\u252b',  # Heavy vertical and left
    0x000ff666: u'\u2533',  # Heavy down and horizontal
    0x666ff000: u'\u253b',  # Heavy up and horizontal
    0x666ff666: u'\u254b',  # Heavy cross

    0x000cc000: u'\u2578',  # Bold horizontal left
    0x00066000: u'\u2579',  # Bold horizontal up
    0x00033000: u'\u257a',  # Bold horizontal right
    0x00066000: u'\u257b',  # Bold horizontal down

    0x06600660: u'\u254f',  # Heavy double dash vertical

    0x000f0000: u'\u2500',  # Light horizontal
    0x0000f000: u'\u2500',
    0x44444444: u'\u2502',  # Light vertical
    0x22222222: u'\u2502',

    0x000e0000: u'\u2574',  # light left
    0x0000e000: u'\u2574',  # light left
    0x44440000: u'\u2575',  # light up
    0x22220000: u'\u2575',  # light up
    0x00030000: u'\u2576',  # light right
    0x00003000: u'\u2576',  # light right
    0x00004444: u'\u2575',  # light down
    0x00002222: u'\u2575',  # light down

    # Misc technical

    0x44444444: u'\u23a2',  # [ extension
    0x22222222: u'\u23a5',  # ] extension

    # 12345678
    0x0f000000: u'\u23ba',  # Horizontal scanline 1
    0x00f00000: u'\u23bb',  # Horizontal scanline 3
    0x00000f00: u'\u23bc',  # Horizontal scanline 7
    0x000000f0: u'\u23bd',  # Horizontal scanline 9

    # Geometrical shapes. Tricky because some of them are too wide.

    0x00066000: u'\u25aa',  # Black small square
}


def esc(*args):
    """Escape ansi codes."""
    return '\x1b[%sm' % ';'.join(str(arg) for arg in args)


def clamp(val, small, large):
    """Clamp val to a range."""
    return min(max(int(val), small), large)


def rgb_to_tput(rgb):
    """Convert rgb string (like "0, 0, 0") into a list (like [0, 0, 0])."""
    return [clamp(c, 0, 255) for c in rgb.split(',')]


def make_char(c, fg, bg):
    """Return escaped ansi char."""
    if fg[0] == fg[1] == fg[2] == bg[0] == bg[1] == bg[2] == 0:
        return '\x1b[0m '

    return '{}{}{}'.format(
            esc(38, 2, fg[0], fg[1], fg[2]),
            esc(48, 2, bg[0], bg[1], bg[2]),
            c.encode('utf-8'))


def make_percent(num, den):
    """Make a numberator and a denominator into a percentage."""
    return math.floor(100.0 * (float(num) / max(den, 1)))


def handle_pixel(img, x, y):
    """Turn a 4x8 dict of rgb tuples into a single-ansi char."""
    w, h = img.size
    x_offset = min(x + 4, w)
    y_offset = min(y + 8, h)

    max_rgb = [0, 0, 0]
    min_rgb = [255, 255, 255]

    for i in range(x, x_offset):
        for j in range(y, y_offset):
            rgba = img.getpixel((i, j))
            for channel in range(0, 3):
                max_rgb[channel] = max(max_rgb[channel], rgba[channel])
                min_rgb[channel] = min(min_rgb[channel], rgba[channel])

    split_channel = 0
    best_split = 0

    for channel in range(0, 3):
        split = max_rgb[channel] - min_rgb[channel]

        if split > best_split:
            best_split = split
            split_channel = channel

    split_val = min_rgb[split_channel] + best_split / 2

    bits = 0
    bg_color = []
    fg_color = []

    for j in range(y, y_offset):
        for i in range(x, x_offset):
            rgba = img.getpixel((i, j))
            r, g, b, _ = rgba
            bits = bits << 1
            index = rgba[split_channel]
            num = (index & 255)
            if int(num) > split_val:
                bits |= 1
                fg_color.append((r, g, b))
            else:
                bg_color.append((r, g, b))

    avg_bg_rgb = [sum(color) / len(color) for color in zip(*bg_color)]
    avg_fg_rgb = [sum(color) / len(color) for color in zip(*fg_color)]

    if not avg_fg_rgb:
        avg_fg_rgb = [0, 0, 0]

    if not avg_bg_rgb:
        avg_fg_rgb = [0, 0, 0]

    best_diff = sys.maxint
    inverted = False
    for bitmap in list(BITMAPS.keys()):
        xor = bin(bitmap ^ bits)
        diff = xor.count('1')
        if diff < best_diff:
            character = BITMAPS[bitmap]
            best_diff = diff
            inverted = False

        # make sure to & the ~ with 0xffffffff to fill up all 32 bits
        not_xor = bin((~bitmap & 0xffffffff) ^ bits)
        diff = not_xor.count('1')
        if diff < best_diff:
            character = BITMAPS[bitmap]
            best_diff = diff
            inverted = True

    if best_diff > 10:
        inverted = False
        character = u' \u2591\u2592\u2593\u2588'[
                min(4, len(fg_color) * 5 / 32)]

    if inverted:
        tmp = avg_bg_rgb
        avg_bg_rgb = avg_fg_rgb
        avg_fg_rgb = tmp

    return make_char(character, avg_fg_rgb, avg_bg_rgb)


def frame_to_ansi(frame):
    """Convert an image into 4x8 chunks and return ansi."""
    w, h = frame.size

    buf = '\x1b[0m'
    for y in range(0, h, 8):
        for x in range(0, w, 4):
            buf += handle_pixel(frame, x, y)

        buf += '\n'

    return buf


def die(out=sys.stdout, gif=None):
    """Unbork the terminal."""
    if gif:
        os.remove(gif)
    if out.name != '<stdout>':
        print('printf ', file=out, end='')
    print('\x1b[34h\x1b[?25h\x1b[0m\x1b[0m', file=out)
    sys.exit(0)


def is_url(img):
    """True if image is a url."""
    return img.startswith('http://') or img.startswith('https://')


def main():
    ap = argparse.ArgumentParser()

    ap.add_argument('-w', '--width', type=int,
                    help='Width of file to show', default=80)
    ap.add_argument('-f', '--forever', action='store_true',
                    help='Loop forever')
    ap.add_argument('-d', '--delay', type=float, default=0.1,
                    help='The delay between images that make up a gif')
    ap.add_argument('-o', '--output', type=argparse.FileType('wb', 0),
                    help='Generated bash script path - suitable for sourcing '
                         'from your .bashrc', default=sys.stdout)
    ap.add_argument('-s', '--seperator', type=str, default=None,
                    help='Print the seperator between frames of a gif '
                         '(this can be useful if piping output into '
                         'another file or program)')

    ap.add_argument('img', type=str, help='File to show')
    args = ap.parse_args()

    img = args.img

    gif_path = None
    if is_url(img):
        r = requests.get(img, stream=True)
        r.raise_for_status()

        gif = tempfile.NamedTemporaryFile(prefix='gifup-', delete=False)
        gif_path = gif.name
        with open(gif_path, 'w') as f:
            f.write(r.raw.read())

        img = gif.name

    img = Image.open(img)
    img.load()

    w = args.width * 4
    ow, oh = img.size
    h = oh * w / ow
    size = (w, h)

    offset = 0
    frames_filled = False
    total_frames = 0

    while True:
        try:
            img.seek(offset)

            if not frames_filled:
                total_frames += 1
                offset += 1
                continue

            frame = Image.new('RGBA', img.size)
            frame.paste(img, (0, 0), img.convert('RGBA'))
            frame = frame.resize(size)

            fmt = '\rLoading frames: {:.2f}% ({} of {})'
            print(fmt.format(make_percent(offset, total_frames),
                             offset, total_frames),
                  file=sys.stderr,
                  end='')
            if not FRAMES.get(offset):
                FRAMES[offset] = frame_to_ansi(frame)

            offset += 1
        except EOFError:
            if not frames_filled:
                frames_filled = True
                offset = 0

                continue
            break
        except KeyboardInterrupt:
            die(out=args.output, gif=gif_path)

    offset = 0

    # Clear the \r from sys.stderr
    print('', file=sys.stderr)

    # If we're not writing to stdout, we're generating a bash script
    if args.output.name != '<stdout>':
        print('#!/usr/bin/env bash', file=args.output)

    while True:
        try:
            if args.output.name != '<stdout>':
                print('cat <<FILE{}'.format(offset), file=args.output)

            print('\r\x1b[{}A'.format(h), end='', file=args.output)
            print(FRAMES[offset], end='', file=args.output)
            if args.seperator:
                print(args.seperator, file=args.output)

            if args.output.name != '<stdout>':
                print('FILE{}'.format(offset), file=args.output)

            if args.output.name == '<stdout>':
                time.sleep(args.delay)
            else:
                print('sleep {}'.format(args.delay), file=args.output)

            offset += 1
        except KeyError:
            if args.forever and args.output.name == '<stdout>':
                offset = 0
                continue

            if args.output.name != '<stdout>':
                print('\nFILE{}\n'.format(offset), file=args.output)
                print('printf \x1b[H\x1b[J', file=args.output)

            break

        except KeyboardInterrupt:
            break

    die(out=args.output, gif=gif_path)


if __name__ == '__main__':
    main()
