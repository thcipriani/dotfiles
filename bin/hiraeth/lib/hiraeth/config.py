#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import os
import shutil
import subprocess

import git


class Config(object):
    """Handle parsing and creating the gitconfig."""

    def __init__(self, root, args):
        """Initialize config object."""
        self.root = root
        self._overrides = args

        self.config_home = os.environ.get(
            'XDG_CONFIG_HOME', os.path.expanduser('~/.config'))

        self.config_dir = os.path.join(self.config_home, 'git-photo')
        self.config_file = os.path.join(self.config_dir, 'gitconfig')
        self._config = None

    @property
    def config(self):
        if not self._config:
            self._config = git.GitConfigParser(self.config_file)

        return self._config

    @property
    def paths(self):
        """
        Return all paths that have been configured for use with git-photo

        Have to shellout here rather than use gitpython since gitpython config
        seems incapable of handling paths :((
        """
        if not self._paths:
            cmd = [
                'git', 'config', '--file', self.config_file,
                '--get-all', 'photo.path']

            self._paths = map(
                os.path.expanduser, subprocess.check_output(cmd).splitlines())

        return self._paths

    def setup(self):
        """If config file doesn't exist, create from default."""
        default_config = os.path.join(self.root, 'etc', 'git-photo.gitconfig')

        if not os.path.exists(default_config):
            raise IOError(
                'Default config file not found in %s', default_config)

        user_git_config = os.path.join(self.config_home, 'git', 'config')

        if not os.path.exists(user_git_config):
            user_git_config = os.path.expanduser('~/.gitconfig')

        if not os.path.exists(self.config_dir):
            os.makedirs(self.config_dir)

        if not os.path.exists(self.config_file):
            shutil.copy(default_config, self.config_file)

        cmd = 'git config --global --get-all include.path'
        config_paths = subprocess.check_output(cmd.split()).splitlines()
        for path in config_paths:
            if path == self.config_file:
                print 'git-photo config already found in %s' % user_git_config
                return

        cmd = 'git config --global --path --add include.path'.split()
        cmd.append(self.config_file)
        subprocess.check_call(cmd)
        print 'Default git-photo config added to %s' % user_git_config

    def thumb_sizes(self):
        """Return hash of small, medium, and large thumb sizes."""
        return {
            'small': self.get('thumbs.small'),
            'medium': self.get('thumbs.medium'),
            'large': self.get('thumbs.large'),
        }

    def get(self, key, default=None):
        val = self.__getattr__(key)

        if val:
            try:
                return os.path.expanduser(val)
            except AttributeError:
                return val

        return default

    def __getattr__(self, key):
        """The fucking complexity of this is stupid."""
        try:
            return self._overrides[key]
        except KeyError:
            pass

        if not key.startswith('photo.'):
            key = 'photo.{}'.format(key)

        keys = key.split('.')

        if keys[1] == 'path':
            return self.paths

        target = keys.pop()
        if len(keys) > 1:
            key = '{} "{}"'.format(keys[0], ' '.join(keys[1:]))
        else:
            key = keys[0]

        if target == 'thumbs':
            return self.thumb_sizes()

        return self.config.get_value(key, target)
