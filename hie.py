#!/usr/bin/env python

import sys
import os, fnmatch
from glob import glob
import re

directory = sys.argv[1]

def find_files(directory, pattern):
    for root, dirs, files in os.walk(directory):
        for basename in files:
            if fnmatch.fnmatch(basename, pattern):
                filename = os.path.join(root, basename)
                yield filename

files = list(find_files(".", "*.hs")) + list(find_files(".", "*.lhs"))

for f in files:
    parts = [part.replace(".hs", "").replace(".lhs", "") for part in f.split("/") if re.match("^[A-Z]", part) and part != "Setup.hs"]
    
    if not parts:
        continue
    
    module = ".".join(parts)
    ex = "hie export %s %s/%s" % (f, directory, module)
    print ex
    os.system(ex)



