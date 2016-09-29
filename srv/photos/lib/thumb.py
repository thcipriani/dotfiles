#!/usr/bin/env python2
import os
from PIL import Image

dir = os.path.realpath(os.getcwd())

def isNEF(file):
    _, ext = os.path.splitext(file)
    return ext == '.NEF'

files = [f for f in os.listdir(dir) if isNEF(f)]

