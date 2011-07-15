#!/bin/bash
# need to compile
# - tramp
# - doxymacs
# - org-mode
# - gnus
# - Pymacs

TO_COMPILE="tramp doxymacs org-mode gnus Pymacs magit cedet"

set -x 

function smart_compile {
    if [ -f configure.ac]
    then 
        if [ ! -x configure ]
        then autoreconf -fi
        else ./configure
        fi
    fi
    make
}

for submod in $TO_COMPILE
# is the path changed and automatically fixed or not?
do (cd $submod && smart_compile)
done
