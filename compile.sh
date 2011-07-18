#!/bin/bash
# need to compile
# - tramp
# - doxymacs
# - org-mode
# - gnus
# - Pymacs
# FIXME: cedet is not compiling at all

TO_COMPILE="tramp doxymacs org-mode gnus Pymacs magit"
RESULT=.compiled

set -x 

# TODO: make sure everything goes well
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
do
    if ! grep $submod $RESULT
    then (cd $submod && smart_compile && echo $submod >> $RESULT)
    fi
done
