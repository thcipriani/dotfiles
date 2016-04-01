#!/usr/bin/env python3

import random
import time

arm = 'v^<>'
eye = '^Oo*'
body = '\r%s(%s%s)%s'


for t in range(0, 9):
    print(body % (
        random.choice(arm),
        random.choice(eye),
        random.choice(eye),
        random.choice(arm),
    ))
    time.sleep(1)
