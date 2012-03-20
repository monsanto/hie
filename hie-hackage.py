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

def find_files(directory, pattern):
    for root, dirs, files in os.walk(directory):
        for basename in files:
            if fnmatch.fnmatch(basename, pattern):
                filename = os.path.join(root, basename)
                yield filename

files = list(find_files(".", "*.hs")) + list(find_files(".", "*.lhs"))

for f in files:
    parts = [part.replace(".hs", "").replace(".lhs", "")
             for part in f.split("/")
             if re.match("^[A-Z]", part) and part != "Setup.hs"]
    
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
    
        h.write(hie(f, data))
            




