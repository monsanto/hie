#!/usr/bin/env python

import sys
import os, fnmatch

directory = sys.argv[1]

def find_files(directory, pattern):
    for root, dirs, files in os.walk(directory):
        for basename in files:
            if fnmatch.fnmatch(basename, pattern):
                filename = os.path.join(root, basename)
                yield filename

files = list(find_files(".", "*.hs"))

for f in files:
    parts = [part.replace(".hs", "") for part in f.split("/") if part][1:]
    if len(parts) > 1:
        module = ".".join(parts)
        ex = "hie export %s %s/%s" % (f, directory, module)
        print ex
        os.system(ex)



