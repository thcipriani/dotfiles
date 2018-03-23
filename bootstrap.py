#!/usr/bin/env python

import argparse
import os
import subprocess

DRY_RUN = False
BASE_PATH = os.path.dirname(os.path.realpath(__file__))
HOME = os.environ['HOME']

NOPES = (
    'bin',
    'srv',
    'src',
    'etc',
    'bootstrap.py',
    'project',
    'plan',
    'CHANGELOG.md',
    'README.md'
)


NONS = (
    'bin',
    'srv',
    'src',
    'etc',
)


HARDS = (
    'project',
    'plan',
)


def doit(cmd):
    """
    Exec a command
    """
    print(' '.join(cmd))
    if not DRY_RUN:
        print(subprocess.check_output(cmd))


def link_dotfiles():
    """
    Link all the dotfiles worth linking
    """
    dirname, files, dirs = os.walk(BASE_PATH).next()
    dots = files + dirs
    for dot in [dot for dot in dots if dot not in NOPES]:
        if dot.startswith('.'):
            continue

        cmd = ['ln', '-sfT']
        cmd.append(os.path.join(dirname, dot))
        cmd.append(os.path.join(HOME, '.%s' % dot))
        doit(cmd)


def link_nondotfiles():
    """
    For dem non dots
    """
    for dot in NONS:
        cmd = ['ln', '-sfT']
        cmd.append(os.path.join(BASE_PATH, dot))
        cmd.append(os.path.join(HOME, dot))
        doit(cmd)


def hardlink_plans():
    """
    .plan and .project. Link 'em.
    """
    for hard in HARDS:
        cmd = ['ln', '-f']
        cmd.append(os.path.join(BASE_PATH, hard))
        cmd.append(os.path.join(HOME, '.%s' % hard))
        doit(cmd)


def submodules():
    """
    Why do I do this to my dotties?
    """
    cmd = [
        '/usr/bin/git',
        '-C',
        BASE_PATH,
        'submodule',
        'update',
        '--init',
        '--recursive'
    ]
    doit(cmd)


def vimshit():
    """
    Do dat vim shit
    """
    cmd = [
        '/usr/bin/vim',
        '+PluginInstall',
        '+qall'
    ]
    doit(cmd)


def base16shit():
    """
    Do dat base16 shit

    ...is this better than submodules?
    """
    cmd = [
        '/usr/bin/git',
        'clone',
        'https://github.com/chriskempson/base16-shell.git',
        os.path.join(HOME, '.config', 'base16-shell')
    ]
    doit(cmd)


def parse_args():
    """
    I thought I could get away without it, but I can't
    """
    global DRY_RUN
    ap = argparse.ArgumentParser('Setup muh dotfiles')
    ap.add_argument(
        '-t',
        '--test',
        action='store_true',
        help='Dry run')
    args = ap.parse_args()

    if args.test:
        DRY_RUN = True


def main():
    parse_args()
    link_dotfiles()
    link_nondotfiles()
    hardlink_plans()
    submodules()
    vimshit()
    base16shit()


if __name__ == '__main__':
    main()
