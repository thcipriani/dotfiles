#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os

import jinja2
import yaml

from . import image


class PageException(Exception):
    pass


class Page(object):
    images = []

    def __init__(self, path, config):
        self.root_path = os.path.dirname(path)
        with open(path, 'r') as m:
            self.metadata = yaml.load(m.read())

        self.config = config
        template_dir = config.get('templates')
        self.page_template = os.path.join(template_dir,
                                          'page.html.j2')
        self.image_template = os.path.join(template_dir,
                                           'image.html.j2')
        self.load_images()

    def load_images(self):
        count = 0
        for image_dict in self.metadata.get('images', []):
            try:
                image_path = image_dict.keys()[0]
                image_info = image_dict[image_path]
            except AttributeError:
                # I don't want to make a name for all my images it turns out.
                image_path = image_dict
                image_info = image_dict

            img = image.PageImage(os.path.join(self.root_path, image_path),
                                  self, self.config)
            img.info(image_info)
            img.index = count
            self.images.append(img)
            count += 1

    def _generate_html(self):
        html = []
        for img in sorted(self.images, key=lambda x: x.index):
            cfg = {}
            cfg['title'] = img.title
            cfg['caption'] = img.caption
            cfg['img_path'] = img.link_dir
            cfg['thumb_path'] = img.link_size('thumb')
            cfg['copyright'] = img.copyright
            cfg['license'] = img.license
            cfg['licenselink'] = img.license_link

            with open(self.image_template, 'r') as t:
                template = jinja2.Template(t.read())
            html.append(template.render(cfg))

        return '\n'.join(html)

    def _safe(self, string):
        return ''.join([c if c.isalnum() else "_" for c in string]).lower()

    def _img_by_name(self, name):
        for img in self.images:
            if img.title == name:
                return img

        return None

    @property
    def html(self):
        self.metadata.update({'site_name': self.config.get('sitename'),
                              'title': self.title,
                              'body': self._generate_html(),
                              'info': self.info})
        with open(self.page_template, 'r') as t:
            template = jinja2.Template(t.read())
        return template.render(self.metadata)

    @property
    def cover(self):
        img = self.images[0]
        default = img.title
        img = self._img_by_name(self.metadata.get('cover', default))
        try:
            return os.path.join(self.metadata['path'], image.IMG_PATH,
                                img.title, 'small.{}'.format(img.extension))
        except AttributeError:
            import pdb
            pdb.set_trace()

    @property
    def safe_title(self):
        return self._safe(self.title)

    @property
    def info(self):
        return self.metadata.get('info')

    @property
    def title(self):
        return self.metadata.get('title', 'Pictures')

    def generate(self):
        out_dir = self.config.get('public')
        with open(os.path.join(out_dir, 'index.html'), 'w+') as f:
            f.write(self.html)

        for img in self.images:
            img.generate()
