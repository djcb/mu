%global pkg mu
%global pkgname mu-mail-search
%global commit da626cef8093581f4b390c0747acbaf5dc9d4740
%global shortcommit %(c=%{commit}; echo ${c:0:7})
%global gitdate 20190501
%global gittime 0836

Summary: A lightweight email search engine for Maildirs
Name: %{pkgname}
Version: 1.3.1
Release: 0.%{gitdate}%{gittime}git%{shortcommit}%{?dist}
License: GPLv3
Group: Applications/Internet
URL: https://github.com/djcb/mu
Source0: https://github.com/djcb/%{pkg}/archive/%{commit}/%{pkg}-%{shortcommit}.tar.gz
BuildRequires: automake
BuildRequires: gcc
BuildRequires: gcc-c++
BuildRequires: libtool
BuildRequires: xapian-core-devel
BuildRequires: libuuid-devel
BuildRequires: gmime30-devel
BuildRequires: texinfo
Requires: xapian-core
Requires: libuuid
Requires: gmime30
Conflicts: mu

%description
E-mail is the 'flow' in the workflow of many people. Consequently, one spends a lot of time searching for old e-mails, to dig up some important piece of information. With people having tens of thousands of e-mails (or more), this is becoming harder and harder. How to find that one e-mail in an ever-growing haystack?
Enter mu.
'mu' is a set of command-line tools for Linux/Unix that enable you to quickly find the e-mails you are looking for, assuming that you store your e-mails in Maildirs (if you don't know what 'Maildirs' are, you are probably not using them).

%package -n emacs-mu4e
Summary: mu support files for Emacs
BuildArch: noarch
BuildRequires: emacs
Requires: %{pkgname} = %{version}-%{release}
Requires: emacs-filesystem >= %{_emacs_version}

%description -n emacs-mu4e
%{summary}.

%package -n %{pkgname}-guile
Summary: This package contains Guile bindings for %{pkgname}
BuildRequires: guile22-devel
Requires: guile22
Requires: %{pkgname} = %{version}-%{release}

%description -n %{pkgname}-guile
%{summary}.

%prep -n
%setup -q -n %{pkg}-%{commit}

%build
NOCONFIGURE=yes ./autogen.sh
%configure --prefix=/usr

%make_build

# %check
# make check

%install
rm -rf %{buildroot}
%make_install
rm -f %{buildroot}%{_infodir}/dir

# rm -rf $RPM_BUILD_ROOT

%clean
# rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%doc COPYING
%doc
%{_bindir}/mu
%{_mandir}/man*/*
%{_datadir}/doc/%{pkg}/NEWS.org

%files -n emacs-mu4e
%{_emacs_sitelispdir}/mu4e/
%{_infodir}/mu4e.info.gz
%{_datadir}/doc/%{pkg}/mu4e-about.org

%files -n %{pkgname}-guile
%{_libdir}/libguile-%{pkg}*
%{_infodir}/%{pkg}-guile.info.gz
%{_datadir}/guile/site/2.0/%{pkg}.scm
%{_datadir}/guile/site/2.0/%{pkg}/*
%{_datadir}/%{pkg}/scripts/*

%changelog
* Mon May  6 2019 Bojan <bojov@fedoraproject.org> - 1.3.1-0.20190501%{gittime}git%{shortcommit}%{?dist}
- update to latest master

* Tue Mar 19 2019 Bojan <bojov@fedoraproject.org> - 1.0.0-5.20190302%{gittime}git%{shortcommit}%{?dist}
- change package name to mu-mail-search because conflict with mu-editor
- initial package for guile
- bump to latest master at March 2019

* Wed Mar 28 2018 Bojan <bojov@e754.snefu.lnet> - 1.0-4
- bump to latest master
- added gcc and gcc-c++ into BuildRequires (Fedora 29)

* Wed Mar 28 2018 Bojan <bojov@e754.snefu.lnet> - 1.0-2.20180325%{gittime}git%{shortcommit}%{?dist}
- bump to version 1.0 with latest master repo
- build only mu and emacs-mu4e packages
- following Fedora Packaging Guidelines

* Wed Mar 28 2018 Bojan <bojov@e754.snefu.lnet> -
- Initial build.
