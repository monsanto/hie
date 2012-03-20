#!/usr/bin/env python

# hie-hackage.py
# For generating documentation for hackage modules and the hacks necessary...
#
# Christopher Monsanto <chris@monsan.to>
# License: GPLv3 

import sys
import os, fnmatch
from glob import glob
import re
import subprocess

def hie(f, inp):
    k = subprocess.Popen(["hie", f], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    data = k.communicate(inp)[0]
    return data

directory = sys.argv[1]
sources_directory = sys.argv[2]
prefixes = sys.argv[3:] or ["Algebra", "Codec", "Control", "Data", "Database", "Debug", "Foreign", "GHC", "Graphics", "Language", "Numeric", "Network", "Prelude", "Sound", "System", "Test", "Text", "Training"]

def find_files(directory, pattern):
    for root, dirs, files in os.walk(directory):
        for basename in files:
            if fnmatch.fnmatch(basename, pattern):
                filename = os.path.join(root, basename)
                yield filename

files = list(find_files(".", "*.hs")) + list(find_files(".", "*.lhs"))

for f in files:
    path = os.path.abspath(f).replace(sources_directory, "%s/")

    writeit = False
    parts = []
    for part in f.split("/"):
        part = part.replace(".hs", "").replace(".lhs", "")

        if part in prefixes:
            writeit = True
        elif not re.match("^[A-Z]", part):
            parts = []
            writeit = False

        if writeit:
            parts.append(part)

    if not parts:
        continue

    module = ".".join(parts)
    to = "%s/%s" % (directory, module)

    with open(f) as h:
        data = h.read()

    with open(to, "w") as h:
        print
        print f
        print "----------------------"
        print
    
        h.write(hie(path, data))
            




