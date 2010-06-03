# configure.ac --- configuration setup for VM

# Author:        Robert Widhopf-Fenk <hack@robf.de>

# Copyright (C) 2006-2007

# VM is free software; you can redistribute it and/or modify it under the
# terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any later version.

# VM is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
# more details.

# You should have received a copy of the GNU Library General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Process this file with autoconf to produce a new configure script

# VM_ARG_SUBST(VAR, OPTION, VAL, DESC[, DEFAULT[, ACTION]])
#
# Substitute the autoconf variable VAR to a value specified by the user
# option --with-OPTION[=VAL] (described by DESC), or with a DEFAULT value.
# If an additional ACTION is given, it is executed at the top of the
# ACTION-IF-FOUND part of AC_ARG_WITH.
# #### WARNING: pay attention to the quoting of ACTION if given !!!!!
AC_DEFUN([VM_ARG_SUBST],
[
  AC_SUBST([$1])
  AC_ARG_WITH([$2],
    AC_HELP_STRING([--with-][$2]ifelse($3, [], [], [=$3]),
      [$4]ifelse($5, [], [], [ [[[$5]]]])),
    [
      ifelse($6, [], [], $6)
      $1="${withval}"
    ],
    ifelse($5, [], [], [$1="$5"]))
])

# Find a (g)tar program and make sure it is GNU one. A failure is not fatal
# since tar is needed for non critical targets only.
AC_DEFUN([VM_PROG_GNU_TAR],
  [ AC_CHECK_PROGS(TAR, gtar tar)
    if test "x${TAR}" = "xtar" ; then
      AC_MSG_CHECKING([that tar is GNU tar])
      ${TAR} --version > /dev/null 2>&1 || TAR=
      if test "x${TAR}" = "x" ; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT(yes)
      fi
    fi
    if test "x${TAR}" = "x" ; then
      AC_MSG_WARN([*** No GNU tar program found.])
      AC_MSG_WARN([*** Some targets will be unavailable.])
    fi ])

# Find an xargs program. A failure is not fatal, only clean/tarball will not
# work
AC_DEFUN([VM_PROG_XARGS],
  [ AC_CHECK_PROG(XARGS, xargs, xargs)
    if test "x${XARGS}" = "x" ; then
      AC_MSG_WARN([*** No xargs program found.])
      AC_MSG_WARN([*** make clean/tarball will not work.])
    fi ])

# Find a makeinfo program. A failure is not fatal, only info files won't be
# built.
AC_DEFUN([VM_PROG_MAKEINFO],
  [ AC_CHECK_PROG(MAKEINFO, makeinfo, makeinfo)
    if test "x${MAKEINFO}" = "x" ; then
      AC_MSG_WARN([*** No makeinfo program found.])
      AC_MSG_WARN([*** Info files will not be built.])
    fi ])

# Find a texi2dvi program. A failure is not fatal, only dvi and pdf files
# won't be built.
AC_DEFUN([VM_PROG_TEXI2DVI],
  [ AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi)
    if test "x${TEXI2DVI}" = "x" ; then
      AC_MSG_WARN([*** No texi2dvi program found.])
      AC_MSG_WARN([*** DVI and PDF files will not be built.])
    fi ])

# Choose an Emacs flavor according to the --with-emacs user option, or try
# emacs and xemacs.
# We use EMACS_PROG instead of EMACS to avoid colliding with Emacs' own
# internal environment.
AC_DEFUN([VM_PROG_EMACS],
  [ AC_ARG_WITH([emacs],
     AC_HELP_STRING([--with-emacs=PROG],
        [choose which flavor of Emacs to use]),
      [ EMACS_PROG="${withval}" ],
      [ AC_CHECK_PROGS(EMACS_PROG, emacs xemacs) ])
    if test "x${EMACS_PROG}" = "x" ; then
      dnl This is critical enough to generate an error and not a warning...
      AC_MSG_ERROR([*** No Emacs program found.])
    fi
    # EMACS TYPE ##################################################
    AC_MSG_CHECKING([checking emacs-type of ${EMACS_PROG}])
    cat > conftest.el <<TEST
(princ (format "%s" (if (featurep 'xemacs ) 'xemacs 'emacs)))
TEST
    EMACS_FLAVOR=`"${EMACS_PROG}" --no-site-file --batch -l conftest.el | ${GREP} .`
    AC_MSG_RESULT([${EMACS_FLAVOR}])
    # EMACS VERSION ###############################################
    AC_MSG_CHECKING([checking emacs-version of ${EMACS_PROG}])
    cat > conftest.el <<TEST
(princ (format "%d" emacs-major-version))
TEST
    EMACS_VERSION=`"${EMACS_PROG}" --no-site-file --batch -l conftest.el | ${GREP} .`
    if test ${EMACS_VERSION} -lt 21; then
      AC_MSG_ERROR([Emacs version ${EMACS_VERSION} is too old, 21 is minimum!])
    fi
    # EMACS DEPENDENT SETTINGS ####################################
    # We may add a version check here ...
    AC_MSG_RESULT([${EMACS_VERSION}])

    # Copied from gnus aclocal.m4
    AC_ARG_WITH(lispdir,[  --with-lispdir=DIR      where to install lisp files], lispdir=${withval})
    AC_MSG_CHECKING([where .elc files should go])
    if test -z "$lispdir"; then
      dnl Set default value
      theprefix=$prefix
      if test "x$theprefix" = "xNONE"; then
  	theprefix=$ac_default_prefix
      fi
      datadir="\$(prefix)/share"
      if test "$EMACS_FLAVOR" = "xemacs"; then
          datadir="\$(prefix)/lib"
          lispdir="${datadir}/${EMACS_FLAVOR}/site-packages/lisp/vm"
      else
          lispdir="${datadir}/${EMACS_FLAVOR}/site-lisp/vm"
      fi
      for thedir in share lib; do
  	potential=
  	if test -d "${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp"; then
             if test "$EMACS_FLAVOR" = "xemacs"; then
  	         lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/lisp/vm"
             else
                 lispdir="${datadir}/${EMACS_FLAVOR}/site-lisp/vm"
             fi
  	   break
  	fi
      done
    fi
    AC_MSG_RESULT($lispdir)
    AC_SUBST(lispdir)

    AC_ARG_WITH(pixmapdir,[  --with-pixmapdir=DIR    where to install pixmap files], pixmapdir=${withval})
    AC_MSG_CHECKING([where pixmaps should go])
    if test -z "$pixmapdir"; then
      dnl Set default value
      pixmapdir="${datadir}/vm"
    fi
    AC_MSG_RESULT($pixmapdir)
    AC_SUBST(pixmapdir)

#    if test "x${EMACS_FLAVOR}" = "xemacs" ; then
#      PACKAGEDIR="${prefix}/share/emacs/site-lisp"
#    else
#      PACKAGEDIR="${HOME}/.xemacs/xemacs-packages"
#    fi

    AC_SUBST(EMACS_PROG)
    AC_SUBST(EMACS_FLAVOR)
    AC_SUBST(EMACS_VERSION)
])

# copied from gnus aclocal.m4
AC_DEFUN([VM_BUILD_FLAGS], [
  AC_MSG_CHECKING([which options to pass on to (X)Emacs])
  if test "x$FLAGS" = "x"; then
    if test "x$EMACS_FLAVOR" = "xxemacs"; then
      FLAGS="-batch -no-autoloads -l \$(srcdir)/vm-build.el"
    else
      FLAGS="-batch -q -no-site-file -no-init-file -l \$(srcdir)/vm-build.el"
    fi
  else
    FLAGS=$FLAGS
  fi
  AC_MSG_RESULT($FLAGS)
  AC_SUBST(FLAGS)
])


# Copied from gnus aclocal.m4 (AC_PATH_INFO_DIR)
AC_DEFUN([VM_PATH_INFO_DIR], [
  AC_MSG_CHECKING([where the TeXinfo docs should go])
  dnl Set default value.  This must be an absolute path.
  if test "$infodir" = "\${prefix}/info"; then
    if test "$EMACS_FLAVOR" = "xemacs"; then
      info_dir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/info"
    else
      info_dir="\$(prefix)/share/info"
    fi
  else
    info_dir=$infodir
  fi
  AC_MSG_RESULT($info_dir)
  AC_SUBST(info_dir)
])

# --with-other-dir option
AC_DEFUN([VM_OTHERDIRS],
  [ VM_ARG_SUBST([OTHERDIRS], [other-dirs], [DIRS],
    [set other needed lisp directories (a list of semicolon separated paths)],
    [],)
  AC_MSG_CHECKING([otherdirs])
  cat > conftest.el <<TEST
    (let ((otherdirs (delete "" (split-string "${OTHERDIRS}" ";")))
    	  absolutedirs
          dir)
      (while otherdirs
        (setq dir (expand-file-name (vm-fix-cygwin-path (car otherdirs)))
	      otherdirs (cdr otherdirs))
        (if (not (file-exists-p dir))
            (error "Directory %S does not exist!" dir)
          (add-to-list 'absolutedirs dir)))
      (princ (format "%S" absolutedirs)))
TEST
  OTHERDIRS=`"${EMACS_PROG}" --no-site-file --batch -l ${srcdir}/lisp/vm-build.el -l conftest.el`
  AC_MSG_RESULT($OTHERDIRS)
  AC_SUBST(OTHERDIRS)
])

##############################################################################
AC_INIT([VM], [8.1.1], [vm@lists.launchpad.net])
# Name of the application
# Version (release) number
# Contact address

AC_COPYRIGHT([Copyright (C) 2006-2007 Robert Widhopf-Fenk <hack@robf.de>])

AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_FILES([Makefile lisp/Makefile info/Makefile src/Makefile pixmaps/Makefile vm-load.el])

# Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PATH_PROG(RM, rm, /bin/rm)
AC_PATH_PROG(LS, ls, /bin/ls)
AC_PATH_PROG(MKDIR, mkdir, /bin/mkdir)
AC_PATH_PROG(GREP, grep, /bin/grep)

# External programs checking:
VM_PROG_XARGS
VM_PROG_GNU_TAR
VM_PROG_MAKEINFO
VM_PROG_TEXI2DVI

VM_PROG_EMACS
VM_BUILD_FLAGS
VM_PATH_INFO_DIR
VM_OTHERDIRS

# is there a sane way to set this to a useful default?
VM_ARG_SUBST([PACKAGEDIR], [package-dir], [DIR],
  [set the Emacs package directory to DIR],)

VM_ARG_SUBST([SYMLINKS], [symlinks], [],
  [install VM by linking instead of copying], [no])

VM_ARG_SUBST([LINKPATH], [linkpath], [PATH],
  [path to symlink from if `pwd' does not work])

AC_OUTPUT

# configure.ac ends here
