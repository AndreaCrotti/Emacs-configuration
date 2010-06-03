#!/usr/bin/python
# -*- python -*-

import os, sys

if not os.path.exists("gtk"):
    os.mkdir("gtk")

for xpmfile in sys.argv[1:]:
    fd = open(xpmfile, "r")
    xpm = fd.readlines()
    fd.close()
    xpm[2] = xpm[2].replace("32 32", "28 24")
    xpm[3] = xpm[3].replace("#B2B2B2 s backgroundToolBarColor", "none")
    for i in range(4, 20):
        if xpm[i].startswith('" '):
            del xpm[i+22:i+31]
            xpm.insert(i, xpm[i])
            for j in range(i, i+24):
                xpm[j] = '"' + xpm[j][3:3+28] + '"' + xpm[j][34:]
            break
    fd = open("gtk/" + xpmfile, "w")
    fd.writelines(xpm)
    fd.close()
