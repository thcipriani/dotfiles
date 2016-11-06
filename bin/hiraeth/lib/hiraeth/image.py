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
        ]

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
            thumb_name = thumb_names[size]
            out_path = os.path.join(self.config.public, thumb_name)
            public_path = os.path.join(self.config.thumburl, thumb_name)
            print('%s --> %s' % (out_path, public_path))
            utils.mkdir_p(os.path.dirname(out_path))
            utils.resize_image(self.config.path, out_path, side=height)
        self._upload_original()
        print('%s ready to upload to %s' % (self.config.public,
                                            self.config.thumburl))

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
                self._thumbs[size] = self._thumb_name(height)

            self._thumbs['original'] = self._thumb_name()

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

        utils.mkdir_p(self.dir)
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
        output_exif = copy.copy(self.pic.exif)
        del output_exif['thumbs']
        del output_exif['File Type Extension']
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
