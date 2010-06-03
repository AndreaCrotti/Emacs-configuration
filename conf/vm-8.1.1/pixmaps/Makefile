

# no csh please
SHELL = /bin/sh

##############################################################################
# location of required programms
prefix = /usr/local
MKDIR = /bin/mkdir
RM   = /bin/rm
LS   = /bin/ls
INSTALL = /opt/local/bin/ginstall -c
INSTALL_DATA = ${INSTALL} -m 644

srcdir = .
datadir= $(prefix)/share/vm
datarootdir= ${prefix}/share
pixmapdir = $(prefix)/share/vm

SYMLINKS = no
LINKPATH = 

##############################################################################

all:

Makefile: ./Makefile.in
	cd ./..; ./config.status

install: install-pkg

install-pkg: 
	$(MKDIR) -p "$(DESTDIR)$(pixmapdir)"
	for i in `$(LS) *.xpm` ; do                                    \
	    echo "Installing $$i in '$(DESTDIR)$(pixmapdir)'" ;        \
	    $(INSTALL_DATA) $$i "$(DESTDIR)$(pixmapdir)" ;             \
	done ;
	$(MKDIR) -p "$(DESTDIR)$(pixmapdir)/mime"
	for i in `ls mime/*.xpm` ; do                                  \
	    echo "Installing $$i in '$(DESTDIR)$(pixmapdir)'" ;        \
	    $(INSTALL_DATA) $$i "$(DESTDIR)$(pixmapdir)/mime" ;        \
	done ;                                                         
	$(MKDIR) -p "$(DESTDIR)$(pixmapdir)/gtk"
	for i in `ls gtk/*.xpm` ; do                                   \
	    echo "Installing $$i in '$(DESTDIR)$(pixmapdir)'" ;        \
	    $(INSTALL_DATA) $$i "$(DESTDIR)$(pixmapdir)/gtk" ;         \
	done ;                                                         
	@echo VM pixmaps successfully installed\!

##############################################################################
clean:
	-$(RM) -f *~

distclean: clean
	-$(RM) -f Makefile

