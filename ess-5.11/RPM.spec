# This is an RPM spec file that specifies how to package
# ESS for Fedora Core Linux and, possibly, similar systems.
# $Id: emacs-ess.spec,v 1.9 2004/10/15 16:44:43 thor Exp $

%define name emacs-ess
%define version 5.11
%define release 1.tgm
Summary: Emacs Speaks Statistics add-on package for Emacs
Name: %{name}
Version: %{version}
Release: %{release}
Copyright: GPL
Group: Applications/Editors
Source: http://ESS.R-project.org/downloads/ess/ess-%{version}.tar.gz
URL: http://ESS.R-project.org/
Packager: Tom Moertel <tom-rpms@moertel.com>
BuildRoot: %{_tmppath}/%{name}-root
Prefix: %{_prefix}
BuildArchitectures: noarch
BuildRequires: emacs
Requires: emacs

%description
This package provides Emacs Speaks Statistics (ESS), which provides
Emacs-based front ends for popular statistics packages.

ESS provides an intelligent, consistent interface between the user and
the software.  ESS interfaces with S-PLUS, R, SAS, BUGS and other
statistical analysis packages under the Unix, Microsoft Windows, and
Apple Mac OS operating systems.  ESS is a package for the GNU Emacs
and XEmacs text editors whose features ESS uses to streamline the
creation and use of statistical software.  ESS knows the syntax and
grammar of statistical analysis packages and provides consistent
display and editing features based on that knowledge.  ESS assists in
interactive and batch execution of statements written in these
statistical analysis languages.

%prep
%setup -n ess-%{version}
( cd doc && chmod u+w html info ) # fix perms to ensure builddir can be deleted

%build
make

# create an init file that is loaded when a user starts up emacs to
# tell emacs to autoload our package's Emacs code when needed
cat > %{name}-init.el <<"EOF"
;;; Set up %{name} for Emacs.
;;;
;;; This file is automatically loaded by emacs's site-start.el
;;; when you start a new emacs session.

(require 'ess-site)

EOF

# create a README.RPM file to document any quirks of this package
cat > README.RPM <<EOF
README for %{name}-%{version}-%{release} RPM package

Generally, there will be no need to modify your .emacs file in order
to use the features of this package -- they are enabled by default
when you start Emacs.

Cheers,
Tom

--
Tom Moertel <tom-rpms@moertel.com>
EOF


%install
%{__rm} -rf ${RPM_BUILD_ROOT}
INITDIR=${RPM_BUILD_ROOT}%{_datadir}/emacs/site-lisp/site-start.d
PKGLISP=${RPM_BUILD_ROOT}%{_datadir}/emacs/site-lisp/%{name}-%{version}
INFODIR=${RPM_BUILD_ROOT}%{_infodir}
%{__install} -D %{name}-init.el $INITDIR/%{name}-init.el
%{__install} -d $PKGLISP
%{__install} -d $INFODIR
%{__make} install \
          PREFIX=${RPM_BUILD_ROOT}%{_prefix} \
          LISPDIR=$PKGLISP \
          INFODIR=$INFODIR
%{__rm} -f $INFODIR/dir # don't package but instead update in pre and post
%{__cp} -a etc $PKGLISP # tuck ess's /etc into lisp dir for easy transport

# Uncomment to print the README file after install.
#
# %post
# echo
# cat %{_defaultdocdir}/%{name}-%{version}/README.RPM
# echo

%clean
%{__rm} -rf ${RPM_BUILD_ROOT}

%files
%defattr(-,root,root)
%doc README README.RPM ANNOUNCE COPYING VERSION ChangeLog doc
%dir %{_datadir}/emacs/site-lisp/%{name}-%{version}
%{_datadir}/emacs/site-lisp/%{name}-%{version}/*
%{_datadir}/emacs/site-lisp/site-start.d/*
%{_infodir}/*.gz

%post
[ -f /usr/share/info/ess.info.gz ] && \
  /sbin/install-info /usr/share/info/ess.info.gz /usr/share/info/dir || :

%preun
if [ $1 = 0 ]; then
    [ -f /usr/share/info/ess.info.gz ] && \
      /sbin/install-info --delete /usr/share/info/ess.info.gz \
	/usr/share/info/dir || :
fi


%changelog
* Fri Oct 15 2004 Tom Moertel <thor@bifur.lab01.moertel.com> 5.2.3-1.tgm
- Updated to ESS 5.2.3.

* Fri Aug 27 2004 Tom Moertel <thor@bifur.lab01.moertel.com> 5.2.2-3.tgm
- Updated ESS URL.
- Fixed Summary.

* Fri Aug 27 2004 Tom Moertel <thor@bifur.lab01.moertel.com> 5.2.2-2.tgm
- Fixed bug: Forgot to include ESS's etc/ directory.

* Thu Aug 26 2004 Tom Moertel <thor@bifur.lab01.moertel.com> 5.2.2-1.tgm
- Initial build.


