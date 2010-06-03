

# location of required programms
BZR = bzr --no-plugins
TAR = tar
RM   = /bin/rm
XARGS = xargs
prefix = /usr/local
INSTALL = /opt/local/bin/ginstall -c

SUBDIRS = lisp info src pixmaps

##############################################################################

.PHONY: all install clean distclean

all: vm-load.el
	@for i in $(SUBDIRS) ; do ($(MAKE) -C $$i) || exit 1; done

Makefile vm-load.el: %: config.status ./%.in
	./config.status $@

./configure: ./configure.ac
	cd . ; autoconf
	./config.status --recheck

config.status: ./configure
	./config.status --recheck

install:
	@for i in $(SUBDIRS) ; do ($(MAKE) -C $$i install) || exit 1; done

clean:
	@for i in $(SUBDIRS) ; do ($(MAKE) -C $$i clean) || exit 1; done

distclean:
	@for i in $(SUBDIRS) ; do ($(MAKE) -C $$i distclean) || exit 1; done

push:
	$(BZR) push

##############################################################################
PKGDIR =  $(shell pwd)/,,package/
PKGINFO = $(PKGDIR)/lisp/vm/_pkg.el
xemacs-package:
	if [ "xemacs" != "xxemacs" ]; then \
	  echo "ERROR: Current build dir not configured for XEmacs,"; \
	  echo "ERROR: Please re-run configure with --with-emacs=xemacs."; \
	  exit 1; \
	fi
	-$(RM) -rf ,,package
	cd lisp; make PACKAGEDIR=$(PKGDIR)/lisp/vm install-pkg
	cd info; make info_dir=$(PKGDIR)/info install-pkg
	cd src; make info_dir=$(PKGDIR)/bin install-pkg
	echo ";;;###autoload" > $(PKGINFO)
	echo "(package-provide 'vm'" > $(PKGINFO)
	echo "         :version 0.7" >> $(PKGINFO)
	echo '         :author-version "'`$(BZR) revno "."`'"' >> $(PKGINFO)
	echo "         :type 'regular)" >> $(PKGINFO)
	mkdir $(PKGDIR)/pkginfo;
	touch $(PKGDIR)/pkginfo/MANIFEST.vm;
	cd $(PKGDIR); find -type f | cut -c3- > pkginfo/MANIFEST.vm
	cd ,,package; $(TAR) -cvzf ../vm-pkg.tar.gz *

##############################################################################
release::
	./release.sh

snapshot::
	./release.sh snapshot
