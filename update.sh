#!/usr/bin/env bash

EMACS=$EMACS24
EMACS_PATH=/Applications/Emacs24.app
LISP_DIR=$EMACS_PATH/Contents/Resources/site-lisp

(cd org-mode && make EMACS=$EMACS24 prefix=$EMACS_PATH && make info)

(cd auctex && 
    ./autogen.sh &&
    ./configure --with-emacs=$EMACS24 --with-lispdir=$LISP_DIR &&
    make
)

(cd gnus && ./configure --with-emacs=$EMACS24 && make)

(cd doxymacs && autoreconf -fi && ./configure && make EMACS=$EMACS24)

(cd magit && make)
