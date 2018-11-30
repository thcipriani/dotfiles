#!/usr/bin/env python

import hashlib
import json
import os
import re
import subprocess
import sys

from datetime import date


PHOTO_PATH = os.path.join(os.environ['HOME'], 'src', 'blog', 'photos')
PHOTO_SITE = 'https://photos.tylercipriani.com/thumbs'

FMT = '''[[!meta date="{date}"]]
[[!meta author="Tyler Cipriani"]]
[[!meta copyright="""
Copyright &copy; {year} Tyler Cipriani
"""]]
[[!meta title="{title}"]]
[[!tag
photos
{tags}
]]

![{links}]({large_thumb})

{description}

EXIF |
--- | ---
{exif_table}
'''


TAGS = (
    '-Title',
    '-Description',
    '-CreateDate',
    '-Model',
    '-Make',
    '-Lens',
    '-FocalLength',
    '-ExposureTime',
    '-FNumber',
    '-ISO',
    '-gpslatitude',
    '-gpslongitude',
)


def format_gps_info(exif):
    """
    gps info added if available
    """
    if not exif.get('GPSLatitude'):
        return ''

    link_fmt = (
        'GPS Link | [View in OpenStreepMap]'
        '(https://www.openstreetmap.org/?mlat={}&mlon={}&zoom=19&layers=M)')

    return link_fmt.format(exif.get('GPSLatitude'), exif.get('GPSLongitude'))


def get_thumbs(photo):
    """
    get thumbnail path for photo
    """
    ext = photo.split('.')[-1]
    hash_md5 = hashlib.md5()
    with open(photo, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)

    md5sum = hash_md5.hexdigest()
    path = os.path.join(PHOTO_SITE, md5sum[:2], md5sum[2:])
    return {
        'small': os.path.join(path, 'small.jpg'),
        'medium': os.path.join(path, 'medium.jpg'),
        'large': os.path.join(path, 'large.jpg'),
        'xlarge': os.path.join(path, 'xlarge.jpg'),
        'original': os.path.join(path, 'original.%s' % ext)
    }


def get_exif(photo):
    """
    get exif for photo
    """
    cmd = [
        '/usr/bin/exiftool',
        '-d',
        '%Y-%m-%d %H:%M:%S',
        '-coordFormat',
        '%+.6f',
        '-json',
    ]
    cmd += TAGS
    cmd.append(photo)
    for_some_reason_a_list = json.loads(subprocess.check_output(cmd))
    return for_some_reason_a_list[0]


def get_title(exif):
    """
    prompt for title if not set
    """
    global TITLE
    title = exif.get('title')
    if not title:
        title = raw_input('Enter title for blog post: ').strip()

    return title


def page_escaped_path(exif):
    """
    get title escaped path for page
    """
    title = get_title(exif).lower().encode('utf8').decode('ascii', 'ignore')
    page_name = re.sub('[^0-9a-zA-Z]+', '-', title)
    return os.path.join(PHOTO_PATH, '{}.mdwn'.format(page_name.decode('utf8')))


def exif_blog_tags(exif):
    """
    make blog tags from exif info
    """

    tags = []

    if exif.get('Lens'):
        lens_tag = re.sub('[^0-9a-zA-Z.]+', '-', exif['Lens'].lower())
        tags.append(os.path.join('photos', lens_tag))

    if exif.get('Model'):
        model_tag = re.sub('[^0-9a-zA-Z.]+', '-', exif['Model'].lower())
        tags.append(os.path.join('photos', model_tag))

    return '\n'.join(tags)


def make_thumb_links(thumbs):
    """
    make formated thumbnail links
    """

    out = []

    for size, path in thumbs.items():
        out.append('[{}]({})'.format(size.title(), path))

    return ' |\n'.join(out)


def make_exif_table(exif):
    """
    Make markdown table from exif data
    """
    table = []

    for k, v in exif.items():
        if k == 'SourceFile':
            continue
        if isinstance(v, unicode):
            v = v.encode('utf8')
        table.append("{} | {}".format(k, v))

    table.append(format_gps_info(exif))

    return '\n'.join(table)


def make_page(photo, thumbs, exif):
    """
    create the page in the final location
    """
    path = page_escaped_path(exif)
    print('Making page %s' % path)
    cur_year = date.today().year
    page_content = FMT.format(
        title=get_title(exif).encode('utf8'),
        description=exif.get('Description', ''),
        date=exif['CreateDate'],
        year=cur_year,
        tags=exif_blog_tags(exif),
        links=make_thumb_links(thumbs),
        large_thumb=thumbs['large'],
        exif_table=make_exif_table(exif)
    )
    with open(path, 'w') as f:
        f.write(page_content)


def main():
    """
    Build a blog page!
    """
    if not os.path.exists(PHOTO_PATH):
        raise RuntimeError('%s is not a path!', PHOTO_PATH)

    for photo in sys.argv[1:]:
        thumbs = get_thumbs(photo)
        exif = get_exif(photo)
        make_page(photo, thumbs, exif)


if __name__ == '__main__':
    main()
