#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from __future__ import print_function

import argparse
import fnmatch
import os
import shutil
import sys

from . import config
from . import index
from . import page
from . import utils


class App(object):
    def __init__(self, config):
        self.config = config

        self.pic_dir = self.config.get('pic_dir')
        self.publish_dir = self.config.get('publish_dir')
        self.metadata_file = self.config.get('metadata_file')
        self.static_dir = self.config.get('static_dir')
        self.paths = []
        self.sub_pages = []

        self.mkdirs()

    def mkdirs(self):
        """Make the output directory if it doesn't exist"""
        utils.mkdir_p(self.publish_dir)
        for root, _, filenames in os.walk(self.static_dir):
            for filename in filenames:
                shutil.copy(os.path.join(root, filename),
                            self.publish_dir)

    def get_subdirs(self):
        matches = []
        for root, dirnames, filenames in os.walk(self.pic_dir):
            for filename in fnmatch.filter(filenames, self.metadata_file):
                matches.append(os.path.join(root, filename))

        return matches

    def get_subpages(self):
        pages = []
        for metadata_path in self.paths:
            pages.append(page.Page(metadata_path, self.config))

        return pages

    def main(self, index_page=False):
        self.paths = self.get_subdirs()
        self.sub_pages = self.get_subpages()

        if index_page:
            gallery = index.Gallery(self.sub_pages, self.config)
            gallery.generate()
            return 0

        for subpage in self.sub_pages:
            subpage.generate()

        return 0

    @classmethod
    def index(cls, root_path):
        ap = argparse.ArgumentParser()
        ap.add_argument('-c', '--config', help='Config file')
        args = ap.parse_args()
        args = vars(args)
        args['is_index'] = True

        app = cls(config.Config(root_path, args))
        sys.exit(app.main(index_page=True))

    @classmethod
    def run(cls, root_path):
        ap = argparse.ArgumentParser()
        ap.add_argument('-c', '--config', help='Config file')
        ap.add_argument('pic_dir', help='Source directory')
        ap.add_argument('publish_dir', help='Output directory')
        args = ap.parse_args()

        app = cls(config.Config(root_path, vars(args)))
        sys.exit(app.main())

if __name__ == '__main__':
    App.run()
