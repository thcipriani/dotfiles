#!/usr/bin/env python

from __future__ import print_function

import os
import sys

for cur, _, files in os.walk('blog'):
    for f in files:
        if not f.endswith('.md'):
            continue

        path = os.path.join(cur, f)

        with open(path, 'r') as fn:
            content = fn.read()

        newfile = []
        for line in content.splitlines():
            newline = ''
            if '{% highlight' in line:
                newline = line[len('{% highlight'):]
            if '{%highlight' in line:
                newline = line[len('{%highlight'):]

            if newline:
                newline = newline.strip()

                if newline.endswith('% }'):
                    newline = newline[:-(len('% }'))]
                if newline.endswith('%}'):
                    newline = newline[:-(len('%}'))]

                newline = newline.strip()
                linenos = ' '
                if newline.endswith('lineos'):
                    newline = newline[:-(len('lineos'))]
                    linenos = ' linenos=yes '

                newline = '[[!pygments{}lexer={} content="""'.format(
                    linenos, newline)

            if '{% endhighlight %}' in line:
                newline = '"""]]'
            if '{%endhighlight%}' in line:
                newline = '"""]]'

            if newline:
                newfile.append(newline)
                continue
            else:
                newfile.append(line)
                continue

        with open(path, 'w') as fn:
            fn.write('\n'.join(newfile))
