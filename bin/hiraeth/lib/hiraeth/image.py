#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from __future__ import print_function

import copy
import hashlib
import os
import re
import subprocess

import jinja2
import shutil
import yaml

from datetime import date

from . import utils


IMG_PATH = 'images'


class Pic(object):
    """
    This is the object that stores information about the image.

    Also, this is the object to call out to to make thumbnails, et al.

    Basically, this object will sometimes have to stand-in for the image
    not actually existing on disk, since this repo is git-annex-backed.
    """

    def __init__(self, img_path, config):
        """Initialize picture."""
        self.path = img_path
        self.config = config
        self._extension = None
        self._exif = {}
        self._thumbs = {}

        self._exif_slots = [
            'Make',
            'Camera Model Name',
            'Lens',
            'Focal Length',
            'Exposure Time',
            'F Number',
            'ISO',
            'Create Date',
            'File Type Extension',
            'File Name',
            'Title',
            'Comment',
        ]

    @property
    def date(self):
        """Return create date of image for sorting."""
        return self.exif.get('Create Date', '1969:12:31 00:00:00.00')

    @property
    def exif(self):
        """
        Load exif data.

        Try to load exif data from disk first, perferring git annex before
        attempting to use exiftool.
        """
        if not self._exif:
            cmd = 'git annex metadata'.split()
            cmd.append(self.path)
            cmd += '--get exif'.split()
            exifdata = subprocess.check_output(cmd)

            if not exifdata:
                if os.path.exists(os.path.realpath(self.path)):
                    return self._load_exif()
                else:
                    raise IOError(
                        'No metadata could be gleaned from %s', self.path)

            # the range here is to remove the ""\n that surrounds
            # the good stuff
            self._exif = yaml.load(exifdata[1:-2])

        return self._exif

    @property
    def extension(self):
        """Get memoized file extension."""
        if not self._extension:
            self._extension = self.exif['File Type Extension']

        return self._extension

    @property
    def original(self):
        """Get original path name."""
        return self.thumb()

    def thumb(self, size='original'):
        """
        Get thumb path.

        Like _thumb_name, except for use when file doesn't exist on disk
        and the md5 can't just be recalculated.
        """
        return self.exif['thumbs'][size]

    def make_thumbs(self):
        """Make thumbnail files."""
        utils.mkdir_p(self.config.public)
        thumb_names = self._thumb_names()
        for size, height in self.config.get('thumbs').iteritems():
            thumb_name = thumb_names[size]['name']
            out_path = os.path.join(self.config.public, thumb_name)
            public_path = os.path.join(self.config.thumburl, thumb_name)
            print('%s --> %s' % (out_path, public_path))
            utils.mkdir_p(os.path.dirname(out_path))
            utils.resize_image(self.config.path, out_path, side=height)
            self._make_page(out_path, size)
        self._upload_original()
        print('%s ready to upload to %s' % (self.config.public,
                                            self.config.thumburl))

    def _make_page(self, out_path, size='original'):
        """Generate thumbnail pages."""
        output_exif = copy.copy(self.exif)
        del output_exif['thumbs']
        del output_exif['File Type Extension']
        del output_exif['File Name']

        if output_exif.get('Title'):
            del output_exif['Title']

        if output_exif.get('Comment'):
            del output_exif['Comment']

        copyright_holder = self.config.get('copyright')

        if copyright_holder:
            year = date.today().year
            self.copyright = 'Copyright&#169; {} {}'.format(year,
                                                            copyright_holder)

        self.license = self.config.get('license')
        self.license_link = self.config.get('licenselink')

        cfg = {}
        cfg['site_name'] = self.config.get('sitename')
        cfg['size'] = size
        # By design, we don't involve _metadata.yaml, so just guess
        cfg['title'] = self._guess_title()

        caption = self._guess_caption()
        if caption:
            cfg['caption'] = caption

        cfg['original'] = os.path.basename(self._thumb_name())
        cfg['image'] = os.path.basename(out_path)
        cfg['exif'] = output_exif
        cfg['copyright'] = self.copyright
        cfg['license'] = self.license
        cfg['license_link'] = self.license_link

        page_path = os.path.dirname(out_path)
        page_name = '{}.html'.format(size)
        if size == 'medium':
            page_name = 'index.html'

        thumb_page = os.path.join(page_path, page_name)

        template = os.path.join(
            self.config.get('templates'), 'image-page.html.j2')

        with open(template, 'r') as t:
            template = jinja2.Template(t.read())

        with open(thumb_page, 'w+') as f:
            f.write(template.render(cfg))

    def _guess_title(self):
        """
        Guess a title.

        Soooo a pic doesn't necissarily have an Image, but *now* I want to
        build a page as part of uploading thumbs. The easiest thing to do
        here is to just guess at a title. If I care, I can set the filename
        or I can set the exif title correctly.
        """
        fn = self.exif.get('Title', self.exif.get('File Name', None))

        if not fn:
            # Won't even guess here, should maybe be an exception
            return '¯\_(ツ)_/¯'

        # If the we're using just the filename, remove the extension
        if fn.endswith(self.extension):
            return fn[:-(len('.' + self.extension))]

        return fn

    def _guess_caption(self):
        """
        Guess a caption.

        Uses the exif comment, e.g. exiftool -Comment[="whatevs"] [img]
        """
        return self.exif.get('Comment')

    def _upload_original(self):
        """Upload original to thumb site."""
        thumb_name = self._thumb_name()
        out_path = os.path.join(self.config.public, thumb_name)
        public_path = os.path.join(self.config.thumburl, thumb_name)
        print('%s --> %s' % (out_path, public_path))
        utils.mkdir_p(os.path.dirname(out_path))
        shutil.copy(self.config.path, out_path)

    def _cache_exif(self):
        """
        Store exif data.

        Caches exif data in git annex for future fun.
        """
        cmd = 'git annex metadata'.split()
        cmd.append(self.path)
        cmd.append('--set')
        cmd.append('exif="{}"'.format(self._exif))
        subprocess.check_call(cmd)

    def _load_exif(self):
        """Get exif, stupidly."""
        cmd = ['exiftool', self.path]
        out = subprocess.check_output(cmd)
        for line in out.splitlines():
            arr = line.split(':')
            k, v = (arr[0], ':'.join(arr[1:]))
            k = k.strip()
            v = v.strip()

            if k in self._exif_slots:
                self._exif[k] = v

        self._exif['thumbs'] = self._thumb_names()
        self._cache_exif()

        return self._exif

    def _thumb_names(self):
        """Get thumb names, memoized if possible."""
        if not self._thumbs:
            for size, height in self.config.get('thumbs').iteritems():
                w, h, img = utils.calculate_size(self.config.path, height)
                self._thumbs[size] = {
                    'name': self._thumb_name(height),
                    'width': w,
                    'height': h,
                }

            w, h, img = utils.calculate_size(self.config.path)
            w, h = img.size
            self._thumbs['original'] = {
                'name': self._thumb_name(),
                'width': w,
                'height': h,
            }

        return self._thumbs

    def _thumb_name(self, size='original'):
        h = hashlib.new('sha1')
        with open(self.config.path, 'rb') as f:
            h.update(f.read())
        sha1sum = h.hexdigest()

        bn = os.path.splitext(self.config.path)[0]
        path = re.sub(r'[^-\w]', '-', bn.lower())
        fn = '{}-{}.{}'.format(path, size, self.extension)

        return os.path.join(sha1sum[:2], sha1sum[2:], fn)


class PageImage(object):

    index = 0

    def __init__(self, img_path, page, config):
        self.src_path = img_path
        self.page = page
        self.config = config

        self.title = None
        self.caption = ''

        self.template = os.path.join(self.config.get('templates'),
                                     'image-page.html.j2')

        self.pic = Pic(self.src_path, config)

    def info(self, info):
        try:
            self.title = info['title']
        except TypeError:
            self.title = info

        try:
            self.caption = info['caption']
        except (TypeError, KeyError):
            self.caption = ''

        self.dir = os.path.join(self.config.get('public'),
                                IMG_PATH, self.title)

        self.link_dir = os.path.join(IMG_PATH, self.title)
        self.index_path = os.path.join(self.dir, 'index.html')
        self.small_path = os.path.join(self.dir, 'small.html')
        self.large_path = os.path.join(self.dir, 'large.html')

        self.original_path = self.thumb_path()

        copyright_holder = self.config.get('copyright')

        if copyright_holder:
            year = date.today().year
            self.copyright = 'Copyright&#169; {} {}'.format(year,
                                                            copyright_holder)

        self.license = self.config.get('license')
        self.license_link = self.config.get('licenselink')

    def get_next(self, size='medium'):
        try:
            img = self.page.images[self.index + 1]
        except IndexError:
            img = self.page.images[0]

        page = '{}.html'.format(size)
        if size == 'medium':
            page = 'index.html'

        return '../../{}/{}'.format(img.link_dir, page)

    def get_prev(self, size='medium'):
        try:
            img = self.page.images[self.index - 1]
        except IndexError:
            return None

        page = '{}.html'.format(size)
        if size == 'medium':
            page = 'index.html'

        return '../../{}/{}'.format(img.link_dir, page)

    @property
    def extension(self):
        return self.pic.extension

    def thumb_path(self, size='original'):
        """Return thumb path as url."""
        if size == 'thumb':
            size = 'small'
        thumburl = self.config.get('thumburl')
        return os.path.join(thumburl, self.pic.thumb(size))

    def generate(self):
        utils.mkdir_p(self.dir)
        output_exif = copy.copy(self.pic.exif)
        del output_exif['thumbs']
        del output_exif['File Type Extension']
        del output_exif['File Name']

        if output_exif.get('Title'):
            del output_exif['Title']

        if output_exif.get('Comment'):
            del output_exif['Comment']

        cfg = {}
        cfg['site_name'] = self.config.get('sitename')
        cfg['size'] = 'medium'
        cfg['title'] = self.title
        cfg['gallery'] = self.page.title
        cfg['original'] = self.original_path
        cfg['image'] = self.thumb_path('medium')
        cfg['caption'] = self.caption
        cfg['exif'] = output_exif
        cfg['next'] = self.get_next()
        cfg['prev'] = self.get_prev()
        cfg['copyright'] = self.copyright
        cfg['license'] = self.license
        cfg['license_link'] = self.license_link

        with open(self.template, 'r') as t:
            template = jinja2.Template(t.read())

        with open(self.index_path, 'w+') as f:
            f.write(template.render(cfg))

        with open(self.small_path, 'w+') as f:
            cfg['image'] = self.thumb_path('small')
            cfg['next'] = self.get_next('small')
            cfg['prev'] = self.get_prev('small')
            f.write(template.render(cfg))

        with open(self.large_path, 'w+') as f:
            cfg['image'] = self.thumb_path('large')
            cfg['next'] = self.get_next('large')
            cfg['prev'] = self.get_prev('large')
            f.write(template.render(cfg))
