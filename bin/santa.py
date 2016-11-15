#!/usr/bin/env python

from __future__ import print_function

import random


DONT_PAIR = [('blazey', 'tyler')]

EVERYONE = ['tyler', 'shawnie', 'lillian', 'mike', 'blazey']
MATCHES = EVERYONE[:]


def find_match(person, dont_match):
    """Match a person with another person."""
    match = random.choice(MATCHES)

    if match in dont_match or match == person:
        if len(MATCHES) == 1:
            msg = 'Can\'t match %s --> %s; try this again'
            raise RuntimeError(msg, person, match)

        return find_match(person, dont_match)

    MATCHES.pop(MATCHES.index(match))

    print("%s ---> %s" % (person, match))
    return (person, match)

if __name__ == '__main__':
    final_pairing = []
    for person in EVERYONE:
        for pairs in DONT_PAIR:
            dont_match = []
            if person in pairs:
                for match in pairs:
                    if match == person:
                        continue
                    dont_match.append(match)
        final_pairing.append(find_match(person, dont_match))

    print(final_pairing)
