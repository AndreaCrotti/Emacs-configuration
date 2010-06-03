

##############################################################################
# no csh please
SHELL = /bin/sh

SOURCES = $(wildcard *.c)

OBJECTS = $(SOURCES:.c=.o)

##############################################################################
# location of required programms
prefix = /usr/local
exec_prefix = ${prefix}
MKDIR = /bin/mkdir
RM   = /bin/rm
INSTALL = /opt/local/bin/ginstall -c
INSTALL_PROGRAM = ${INSTALL}

prefix = /usr/local
top_srcdir = ..
srcdir = .
bindir= ${exec_prefix}/bin

##############################################################################
all: $(SOURCES:.c=)

install:
	@mkdir -p -m 0755 "$(DESTDIR)$(bindir)";                             \
	for i in $(SOURCES:.c=) ; do                               \
	  echo "Installing $$i in $(DESTDIR)$(bindir)" ;           \
          $(INSTALL_PROGRAM) $$i "$(DESTDIR)$(bindir)" ;           \
        done ;
	@echo VM helper binaries successfully installed\!

##############################################################################
Makefile: ./Makefile.in
	cd ..; ./config.status

##############################################################################
clean:
	-$(RM) -f $(SOURCES:.c=)

distclean: clean
	-$(RM) -f Makefile
