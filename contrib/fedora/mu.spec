%global pkg mu
%global pkgname mu
%global commit 16fa5d9a22e3fbe1787ffd2e00978d442d0c9aa5
%global shortcommit %(c=%{commit}; echo ${c:0:7})
%global gitdate 20180818
%global gittime 1137

Summary: A lightweight email search engine for Maildirs
Name: mu
Version: 1.0
Release: 2.%{gitdate}%{gittime}git%{shortcommit}%{?dist}
License: GPLv3
Group: Applications/Internet
URL: https://github.com/djcb/mu
Source0: https://github.com/djcb/%{pkgname}/archive/%{commit}/%{pkgname}-%{shortcommit}.tar.gz
BuildRequires: automake
BuildRequires: libtool
BuildRequires: xapian-core-devel
BuildRequires: libuuid-devel
BuildRequires: gmime-devel
BuildRequires: texinfo
Requires: xapian-core
Requires: libuuid
Requires: gmime

%description
E-mail is the 'flow' in the work flow of many people. Consequently, one spends a lot of time searching for old e-mails, to dig up some important piece of information. With people having tens of thousands of e-mails (or more), this is becoming harder and harder. How to find that one e-mail in an ever-growing haystack?
Enter mu.
'mu' is a set of command-line tools for Linux/Unix that enable you to quickly find the e-mails you are looking for, assuming that you store your e-mails in Maildirs (if you don't know what 'Maildirs' are, you are probably not using them). 

%package -n emacs-mu4e
Group:   Applications/Editors
Summary: mu support files for Emacs
BuildArch: noarch
BuildRequires: emacs
Requires: %{pkgname} = %{version}-%{release}
Requires: emacs(bin) >= %{_emacs_version}

%description -n emacs-mu4e
%{summary}.


%prep -n
%setup -q -n %{name}-%{commit}

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
%{_datadir}/doc/%{pkgname}/NEWS.org

%files -n emacs-mu4e
%{_emacs_sitelispdir}/mu4e/
%{_infodir}/mu4e.info.gz
%{_datadir}/doc/%{pkgname}/mu4e-about.org

%changelog
* Wed Mar 28 2018 Bojan <bojov@e754.snefu.lnet> - 1.0-2.20180325%{gittime}git%{shortcommit}%{?dist}
- bump to version 1.0 with latest master repo
- build only mu and emacs-mu4e packages
- following Fedora Packaging Guidelines

* Wed Mar 28 2018 Bojan <bojov@e754.snefu.lnet> - 
- Initial build.



