#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os

import yaml

class Config(object):
    DEFAULTS = {
        'pic_dir': os.path.expanduser('~/Pictures'),
        'publish_dir': os.path.expanduser('~/public_html'),
        'metadata_file': '_metadata.y*ml',
        'site_name': 'Muh Photos',
        'thumbs': {
            'small': 160,
            'medium': 400,
            'large': 800,
        }
    }

    def __init__(self, root, args):
        config_file = os.path.join(root, 'etc', 'config.yaml')

        if args.get('config'):
            config_file = args.get('config')

        self.DEFAULTS.update({
            'config_file': config_file,
            'template_dir': os.path.join(root, 'lib', 'templates'),
            'static_dir': os.path.join(root, 'lib', 'static'),
        })

        conf_file = os.path.expanduser(self.DEFAULTS['config_file'])

        if not os.path.isfile(conf_file):
            raise RuntimeError('Config not found: ', conf_file)

        with open(conf_file, 'r') as cfg:
            cfg = yaml.load(cfg.read())

        cfg.update(args)
        self.config = cfg

    def get(self, key):
        return self.config.get(key, self.DEFAULTS[key])
