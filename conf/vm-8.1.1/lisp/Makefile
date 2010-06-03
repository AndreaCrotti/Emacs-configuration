

##############################################################################
# no csh please
SHELL = /bin/sh

# the version of this package
PACKAGE_VERSION = 8.1.1

# the list of source files
SOURCES = vm-version.el
SOURCES += vm.el
SOURCES += vm-autoload.el
SOURCES += vm-avirtual.el
SOURCES += vm-biff.el
SOURCES += vm-crypto.el
SOURCES += vm-delete.el
SOURCES += vm-digest.el
SOURCES += vm-edit.el
SOURCES += vm-folder.el
SOURCES += vm-grepmail.el
SOURCES += vm-imap.el
SOURCES += vm-license.el
SOURCES += vm-macro.el
SOURCES += vm-mark.el
SOURCES += vm-menu.el
SOURCES += vm-message.el
SOURCES += vm-message-history.el
SOURCES += vm-mime.el
SOURCES += vm-minibuf.el
SOURCES += vm-misc.el
SOURCES += vm-motion.el
SOURCES += vm-mouse.el
SOURCES += vm-page.el
SOURCES += vm-pcrisis.el
SOURCES += vm-pgg.el
SOURCES += vm-pine.el
SOURCES += vm-pop.el
SOURCES += vm-ps-print.el
SOURCES += vm-reply.el
SOURCES += vm-rfaddons.el
SOURCES += vm-save.el
SOURCES += vm-search.el
SOURCES += vm-serial.el
SOURCES += vm-sort.el
SOURCES += vm-startup.el
SOURCES += vm-summary.el
SOURCES += vm-summary-faces.el
SOURCES += vm-thread.el
SOURCES += vm-toolbar.el
SOURCES += vm-undo.el
SOURCES += vm-user.el
SOURCES += vm-vars.el
SOURCES += vm-vcard.el
SOURCES += vm-version.el
SOURCES += vm-virtual.el
SOURCES += vm-window.el
SOURCES += vm-w3m.el
SOURCES += vm-w3.el

SOURCES += vcard.el
SOURCES += tapestry.el
SOURCES += u-vm-color.el

# to list of object files
ifeq (emacs,emacs)
OBJECTS = vm-autoloads.elc vm-cus-load.elc
else
OBJECTS = auto-autoloads.elc custom-load.elc
endif

OBJECTS += $(SOURCES:.el=.elc)

AUX_FILES = version.txt

INSTALL_FILES += $(OBJECTS:.elc=.el) $(OBJECTS)
INSTALL_FILES += $(AUX_FILES)

# for autoload generation
AUTOLOAD_PACKAGE_NAME = (setq autoload-package-name \"vm\")
AUTOLOAD_FILE = (setq generated-autoload-file \"./auto-autoloads.el\")

##############################################################################
# location of required programms
RM   = /bin/rm
LS   = /bin/ls
XARGS = xargs
INSTALL = /opt/local/bin/ginstall -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_SCRIPT = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644

prefix = /usr/local
top_srcdir = ..
srcdir = .
datadir= $(prefix)/share
datarootdir= ${prefix}/share
lispdir = $(prefix)/share/emacs/site-lisp/vm
pixmapdir= $(prefix)/share/vm

EMACS_PROG = emacs
EMACS_FLAVOR = emacs

FLAGS = -batch -q -no-site-file -no-init-file -l $(srcdir)/vm-build.el

EMACS_COMP = lispdir="$(lispdir)" srcdir="$(srcdir)" "$(EMACS_PROG)" $(FLAGS)

export OTHERDIRS = nil

SYMLINKS = no
LINKPATH = 

.el.elc:
	"$(EMACS_PROG)" $(FLAGS) -f batch-byte-compile $<

##############################################################################
all: $(OBJECTS)

install: install-el install-elc install-aux

##############################################################################
vm-version.elc: vm-version.el version.txt

version.txt:
	echo "\"$(PACKAGE_VERSION)\"" > $@

##############################################################################
# GNU Emacs's vm-autoload file
vm-autoloads.el: $(SOURCES:%=./%)
	-$(RM) -f $@
	echo > $@
	(build_dir="`pwd`"; cd "."; \
	 "$(EMACS_PROG)" $(FLAGS) -l autoload \
		-f vm-built-autoloads "/Users/andrea/Documents/pycon/conf/vm-8.1.1/lisp/$@" "`pwd`")
	echo "(custom-add-load 'vm 'vm-cus-load)" >> $@
	echo "(setq vm-configure-datadir \"${datadir}\")" >> $@
	echo "(setq vm-configure-pixmapdir \"${pixmapdir}\")" >> $@
	echo "(require 'vm-vars)" >> $@
	echo "(provide 'vm-autoloads)" >> $@

vm-cus-load.el: $(SOURCES:%=./%)
	"$(EMACS_PROG)" $(FLAGS) -f vm-custom-make-dependencies .
ifeq (22,21)
	sed -e "s/provide 'cus-load/provide 'vm-cus-load/" cus-load.el > $@
	$(RM) cus-load.el
endif

##############################################################################
# XEmacs#s auto-autoloads and custom-load file
auto-autoloads.el: $(SOURCES:%=./%)
	-$(RM) -f $@
#	(build_dir=`pwd`; cd "."; \
#	 $(EMACS_PROG) $(FLAGS) -l autoload \
#		-f vm-built-autoloads "/Users/andrea/Documents/pycon/conf/vm-8.1.1/lisp/$@" "`pwd`")
	"$(EMACS_PROG)" $(FLAGS) \
                -eval "$(AUTOLOAD_PACKAGE_NAME)" \
                -eval "$(AUTOLOAD_FILE)" \
                -l autoload -f batch-update-autoloads $^
#	avoid getting an error about an already loaded vm-autoloads
	mv $@ $@.tmp
	echo "(setq features (delete 'vm-autoloads features))" > $@
	cat $@.tmp >> $@
	echo "(setq features (delete 'vm-autoloads features))" >> $@
	echo "(setq vm-configure-datadir \"${datadir}\")" >> $@
	echo "(setq vm-configure-pixmapdir \"${pixmapdir}\")" >> $@
	$(RM) $@.tmp


custom-load.el: $(SOURCES:%=./%)
	"$(EMACS_PROG)" $(FLAGS) -f vm-custom-make-dependencies .

##############################################################################
install-pkg: all $(INSTALL_FILES)
	@if test "x$(SYMLINKS)" = "xno" ; then                 \
	  mkdir -p -m 0755 $(DESTDIR)$(PACKAGEDIR);            \
	  for i in $(SOURCES:%=./%) $(INSTALL_FILES) ; do  \
	    echo "Installing $$i in $(DESTDIR)$(PACKAGEDIR)" ; \
	    $(INSTALL_DATA) $$i $(DESTDIR)$(PACKAGEDIR) ;      \
	  done ;                                               \
	else                                                   \
	  if test "x$(LINKPATH)" = "x" ; then                  \
	    $(LN_S) "`pwd`" $(DESTDIR)$(PACKAGEDIR) ;          \
	  else                                                 \
	    $(LN_S) $(LINKPATH)/lisp $(DESTDIR)$(PACKAGEDIR) ; \
	  fi ;                                                 \
	fi
	@echo VM ELISP files successfully installed\!

# This entry will never install .el files if there are no .elc files.
install-el: all $(INSTALL_FILES)
	$(INSTALL) -d -m 0755 "$(DESTDIR)$(lispdir)/"
	for elc in *.elc; do                                                  \
	    el=`basename $$elc c`;                                            \
	    if test -f "$(srcdir)/$$el"; then                                 \
	        echo "Install $$el in $(DESTDIR)$(lispdir)/";                 \
	        $(INSTALL_DATA) "${srcdir}/$$el" "$(DESTDIR)$(lispdir)/";     \
	    fi;                                                               \
        done;
	if $(LS) $(contrib)/*.elc > /dev/null 2>&1; then                      \
	    for elc in $(contribdir)/*.elc; do                                \
	        el=`basename $$elc c`;                                        \
	        if test -f "${srcdir}/$(contribdir)/$$el"; then               \
	            echo "Install $(contribdir)/$$el in $(DESTDIR)$(lispdir)/"; \
	            $(INSTALL_DATA) "${srcdir}/$(contribdir)/$$el" "$(DESTDIR)$(lispdir)/"; \
	        fi;                                                           \
	    done;                                                             \
	fi;

install-elc: all $(INSTALL_FILES)
	$(INSTALL) -d -m 0755 "$(DESTDIR)$(lispdir)/"
	for elc in *.elc; do                                 \
	    echo "Install $$elc in $(DESTDIR)$(lispdir)/";   \
	    $(INSTALL_DATA) $$elc "$(DESTDIR)$(lispdir)/";   \
        done;
	if $(LS) $(contribdir)/*.elc > /dev/null 2>&1; then  \
	    for elc in $(contribdir)/*.elc; do               \
	        echo "Install $$elc in $(DESTDIR)$(lispdir)"; \
	        $(INSTALL_DATA) $$elc "$(DESTDIR)$(lispdir)"; \
	    done;                                            \
	fi;

install-aux: $(AUX_FILES)
	$(INSTALL) -d -m 0755 "$(DESTDIR)$(lispdir)/"
	for i in $(AUX_FILES); do                                 \
	    echo "Install $$i in $(DESTDIR)$(lispdir)/";   \
	    $(INSTALL_DATA) $$i "$(DESTDIR)$(lispdir)/";   \
        done;

##############################################################################
Makefile: ./Makefile.in
	cd .. ; ./config.status

##############################################################################
clean:
	-$(RM) -f version.txt *.elc vm-autoloads.el auto-autoloads.el custom-load.el

distclean: clean
	-$(RM) -f Makefile
