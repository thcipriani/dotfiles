#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os

import jinja2


def make(collections, config):
    """Make the galleries page and the most recent page."""
    gallery = Gallery(collections, config)
    gallery.generate()

    home = Home(collections, config)
    home.generate()


class Home(object):
    """Home page!"""

    # The 50 most recent images
    INDEX_IMAGES = 50

    def __init__(self, pages, config):
        """Initialize home page"""
        self.sub_pages = pages
        self.config = config
        self.titles = []

        self.index_path = os.path.join(self.config.get('public'), 'index.html')

        template_dir = self.config.get('templates')
        self.template = os.path.join(template_dir, 'index.html.j2')
        self.image_template = os.path.join(template_dir, 'image.html.j2')

        self.load_paths()

    def load_paths(self):
        all_images = []
        for page in self.sub_pages:
            for image in page.images:
                all_images.append(image)

        # Sort by date created
        all_images = sorted(
            all_images, key=lambda img: img.pic.date, reverse=True)
        all_images = all_images[:self.INDEX_IMAGES]

        for image in all_images:
            link = os.path.join(
                self.config.get('thumburl'), image.pic.thumb('medium'))
            index_link = os.path.dirname(link)
            self.titles.append({
                'title': image.title,
                'caption': image.caption,
                'img_path': index_link,
                'thumb_path': link})

    def generate(self):
        html = []
        with open(self.image_template, 'r') as t:
            template = jinja2.Template(t.read())

        for title in self.titles:
            html.append(template.render(title))

        with open(self.template, 'r') as t:
            template = jinja2.Template(t.read())

        cfg = {'site_name': self.config.get('sitename'),
               'body': '\n'.join(html)}

        with open(self.index_path, 'w+') as i:
            i.write(template.render(cfg))


class Gallery(object):
    def __init__(self, pages, config):
        self.sub_pages = pages
        self.config = config
        self.titles = []

        self.gallery_path = os.path.join(self.config.get('public'), 'gallery.html')

        template_dir = self.config.get('templates')
        self.template = os.path.join(template_dir, 'index.html.j2')
        self.image_template = os.path.join(template_dir, 'image.html.j2')

        self.load_paths()

    def load_paths(self):
        for page in self.sub_pages:

            self.titles.append({
                'title': page.title,
                'caption': page.info,
                'img_path': page.metadata['path'],
                'thumb_path': page.cover})

    def generate(self):
        html = []
        with open(self.image_template, 'r') as t:
            template = jinja2.Template(t.read())

        for title in self.titles:
            html.append(template.render(title))

        with open(self.template, 'r') as t:
            template = jinja2.Template(t.read())

        cfg = {'site_name': self.config.get('sitename'),
               'body': '\n'.join(html)}

        with open(self.gallery_path, 'w+') as i:
            i.write(template.render(cfg))
