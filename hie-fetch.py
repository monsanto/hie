#!/usr/bin/env python

# hie-fetch.py
# Fetches installed ghc packages.
#
# Christopher Monsanto <chris@monsan.to>
# License: GPLv3

import sys
import re
import subprocess

blacklisted = sys.argv[1:] + ["haskell98", "haskell2010", "bin-package-db", "ghc-binary",
                              "integer-gmp", "rts", "ghc", "ghc-prim"]

packages = {}

for package in subprocess.check_output(["ghc-pkg", "list"]).split("\n"):
    m = re.match("^[ ]+([A-Za-z0-9_-]+)-([0-9.]+)$", package)
    if m:
        pkg = m.group(1)
        version = m.group(2)
        if pkg not in blacklisted:
            packages[pkg] = version  # this always picks the latest package
            
for k, v in packages.iteritems(): 
    subprocess.call(["cabal", "unpack", "%s-%s" % (k, v)]) 
        
    
