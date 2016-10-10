#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os

import jinja2


class Gallery(object):
    def __init__(self, pages, config):
        self.sub_pages = pages
        self.config = config
        self.titles = []

        self.index_path = os.path.join(self.config.get('public'),
                                       'index.html')

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

        cfg = {'site_name': self.config.get('site_name'),
               'body': '\n'.join(html)}

        with open(self.index_path, 'w+') as i:
            i.write(template.render(cfg))
