#!/usr/bin/env python

import argparse
import hashlib
import json
import logging
import os
import subprocess
import sys

from datetime import datetime

DRY_RUN = False

TAGS = (
    '-Title',
    '-Description',
    '-Comment',
    '-FileName',
    '-Model',
    '-Make',
    '-Lens',
    '-FocalLength',
    '-ExposureTime',
    '-FNumber',
    '-ISO',
    '-CreateDate',
    '-FileTypeExtension',
    '-ColorSpace',
    '-Orientation',
    '-ImageWidth',
    '-ImageHeight',
)


def md5sum(fname):
    hash_md5 = hashlib.md5()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)

    return hash_md5.hexdigest()


def get_exif(path):
    LOG.debug('Get exif for %s' % path)
    cmd = [
        'exiftool',
        '-d',
        '%Y-%m-%d %H:%M:%S',
        '-json',
    ]
    cmd += TAGS
    cmd.append(path)
    LOG.debug(' '.join(cmd))
    exif = json.loads(subprocess.check_output(cmd))

    timestamp = datetime.utcnow()
    date = timestamp.strftime('%F %H:%M:%S')

    del exif[0]['SourceFile']
    exif[0]['Published'] = date
    return exif


def structured_metadata(path):
    """Return a hash that is formatted like git-annex likes."""
    metadata = {'file': path, 'fields': {}}
    exif = get_exif(path)[0]
    LOG.debug(exif)
    metadata['fields']['exif'] = ['NEW FORMAT']
    for key in exif:
        metadata['fields'][key] = [str(exif[key])]

    metadata['fields']['md5'] = [md5sum(path)]
    return metadata


def tag_file(exif_info):
    """Add exif info to git annex."""
    cmd = ['git', 'annex', 'metadata', '--batch', '--json']
    cmd.append(exif_info['file'])

    exif_json = json.dumps(exif_info)

    if DRY_RUN:
        LOG.info('DRY_RUN: cmd({}); json({})'.format(cmd, exif_json))
        return

    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    out, err = p.communicate(exif_json)

    if err:
        LOG.error(err)
        sys.exit(1)

    x = json.loads(out)
    if x['success']:
        LOG.info('Tagged {}'.format(exif_info['file']))
    else:
        LOG.error('Failed to tag {}'.format(exif_info['file']))
        LOG.error(x)
        sys.exit(1)


def setup_logger(verbose):
    global LOG

    level = logging.INFO

    if verbose:
        level = logging.DEBUG

    logging.basicConfig(format='[%(levelname)s] %(message)s', level=level)
    LOG = logging.getLogger(__name__)


def parse_opts():
    ap = argparse.ArgumentParser()
    ap.add_argument('--verbose', '-v', action='store_true',
                    help='Show debug output')
    ap.add_argument('--dry-run', '-d', action='store_true',
                    help='Show what commands would be run without '
                         'running them')
    ap.add_argument('files', nargs='*', help='Files to annotate')
    return ap.parse_args()


def main():
    global DRY_RUN
    args = parse_opts()
    setup_logger(args.verbose)
    DRY_RUN = args.dry_run
    files = map(os.path.expanduser, map(os.path.expandvars, args.files))
    map(tag_file, [structured_metadata(path) for path in files])


if __name__ == '__main__':
    main()
