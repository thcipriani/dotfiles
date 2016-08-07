#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import subprocess

import jinja2
import shutil

from datetime import date

from PIL import Image

from . import utils


IMG_PATH = 'images'


class PageImage(object):

    index = 0

    def __init__(self, img_path, page, config):
        if not config.get('is_index'):
            self.image = Image.open(img_path)

        self.page = page
        self.src_path = img_path
        self.config = config
        self.path = os.path.join(IMG_PATH,
                                 os.path.basename(img_path))
        self.name = os.path.basename(img_path)

        self.template = os.path.join(self.config.get('template_dir'),
                                     'image-page.html.j2')

        self.exif = {
            'Make': '',
            'Camera Model Name': '',
            'Lens': '',
            'Focal Length': '',
            'Exposure Time': '',
            'F Number': '',
            'ISO': '',
            'Create Date': '',
        }

    def info(self, info):
        try:
            self.title = info['title']
        except TypeError:
            self.title = info

        try:
            self.caption = info['caption']
        except (TypeError, KeyError):
            self.caption = ''

        self.dir = os.path.join(self.config.get('publish_dir'),
                                IMG_PATH, self.title)

        self.link_dir = os.path.join(IMG_PATH, self.title)
        self.index_path = os.path.join(self.dir, 'index.html')
        self.small_path = os.path.join(self.dir, 'small.html')
        self.large_path = os.path.join(self.dir, 'large.html')
        self.original_path = os.path.join(self.dir, self.name)

        copyright_holder = self.config.get('copyright_holder')

        if copyright_holder:
            year = date.today().year
            self.copyright = 'Copyright&#169; {} {}'.format(year,
                                                            copyright_holder)

        self.license = self.config.get('license')
        self.license_link = self.config.get('license_link')

    def make_thumbs(self):
        for size, height in self.config.get('thumbs').iteritems():
            utils.resize_image(self, self.size(size), side=height)

    def link_size(self, size):
        if size == 'thumb':
            size = 'small'

        return os.path.join(self.link_dir, '{}.{}'.format(size,
                                                          self.extension))

    def size(self, size):
        if size == 'thumb':
            size = 'small'

        return os.path.join(self.dir, '{}.{}'.format(size, self.extension))

    @property
    def extension(self):
        if self.config.get('is_index'):
            ext = self.src_path.split('.')[-1]
            if ext.lower() == 'jpg':
                return 'jpeg'
            return ext
        return self.image.format.lower()

    def get_exif(self):
        """This is fucking stupid."""
        cmd = ['exiftool', self.src_path]
        out = subprocess.check_output(cmd)
        for line in out.splitlines():
            arr = line.split(':')
            k, v = (arr[0], ':'.join(arr[1:]))
            k = k.strip()
            v = v.strip()

            if k in self.exif.keys():
                self.exif[k] = v

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

    def generate(self):
        utils.mkdir_p(self.dir)
        shutil.copy(self.src_path, self.original_path)
        self.make_thumbs()
        self.get_exif()

        cfg = {}
        cfg['site_name'] = self.config.get('site_name')
        cfg['size'] = 'medium'
        cfg['title'] = self.title
        cfg['gallery'] = self.page.title
        cfg['original'] = self.name
        cfg['image'] = 'medium.{}'.format(self.extension)
        cfg['caption'] = self.caption
        cfg['exif'] = self.exif
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
            cfg['image'] = 'small.{}'.format(self.extension)
            cfg['next'] = self.get_next('small')
            cfg['prev'] = self.get_prev('small')
            f.write(template.render(cfg))

        with open(self.large_path, 'w+') as f:
            cfg['image'] = 'large.{}'.format(self.extension)
            cfg['next'] = self.get_next('large')
            cfg['prev'] = self.get_prev('large')
            f.write(template.render(cfg))
