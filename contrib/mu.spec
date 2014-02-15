
# These refer to the release version
# When 0.9.9.6 gets out, remove the global pre line
%global pre	pre2
%global rel	1

Summary:	A lightweight email search engine for Maildirs
Name:		mu
Version:	0.9.9.6
URL:		https://github.com/djcb/mu
# From Packaging:NamingGuidelines for pre-relase versions:
# Release: 0.%{X}.%{alphatag} where %{X} is the release number
%if %{pre}
Release:	0.%{rel}.%{prerelease}%{?dist}
%else
Release:	%{rel}%{?dist}
%endif

License:	GPLv3
Group:		Applications/Internet
BuildRoot:	%{_tmppath}/%{name}-%{version}-build

# Source is at ssaavedra repo because djcb has not yet this version tag created
Source0:	http://github.com/ssaavedra/%{name}/archive/v%{version}%{?pre}.tar.gz
BuildRequires:	emacs-el
BuildRequires:	emacs
BuildRequires:	gmime-devel
BuildRequires:	guile-devel
BuildRequires:	xapian-core-devel
BuildRequires:	libuuid-devel
BuildRequires:	texinfo
Requires:	gmime
Requires:	guile
Requires:	xapian-core-libs
Requires:	emacs-filesystem >= %{_emacs_version}


%description
E-mail is the 'flow' in the work flow of many people. Consequently, one spends a lot of time searching for old e-mails, to dig up some important piece of information. With people having tens of thousands of e-mails (or more), this is becoming harder and harder. How to find that one e-mail in an ever-growing haystack?
Enter mu.
'mu' is a set of command-line tools for Linux/Unix that enable you to quickly find the e-mails you are looking for, assuming that you store your e-mails in Maildirs (if you don't know what 'Maildirs' are, you are probably not using them). 

%package gtk
Group:		Applications/Internet
Summary:	GUI for using mu (called mug)
BuildRequires:	gtk3-devel
BuildRequires:	webkitgtk3-devel
Requires:	gtk3
Requires:	gmime
Requires:	webkitgtk3
Requires:	mu = %{version}-%{release}

%description gtk
Mug is a simple GUI for mu from version 0.9.

%package guile
Group:		Applications/Internet
Summary:	Guile scripting capabilities for mu
Requires:	guile
Requires:	mu = %{version}-%{release}
Requires(post):	info
Requires(preun):	info

%description guile
Bindings for Guile to interact with mu.


%prep
%setup -n %{name}-%{version}%{?pre} -q

%build
autoreconf -i
%configure
make %{?_smp_mflags}

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot}
install -p -c -m 755 %{_builddir}/%{buildsubdir}/toys/mug/mug %{buildroot}%{_bindir}/mug
cp -p %{_builddir}/%{buildsubdir}/mu4e/*.el %{buildroot}%{_emacs_sitelispdir}/mu4e/
rm -f %{buildroot}%{_infodir}/dir

%clean
rm -rf %{buildroot}

%post
/sbin/install-info \
	--info-dir=%{_infodir} %{_infodir}/mu4e.info.gz || :
%preun
if [ $1 = 0 -a -f %{_infodir}/mu4e.info.gz ]; then
	/sbin/install-info --delete \
	--info-dir=%{_infodir} %{_infodir}/mu4e.info.gz || :
fi

%post guile
/sbin/install-info \
	--info-dir=%{_infodir} %{_infodir}/mu-guile.info.gz || :

%preun guile
if [ $1 = 0 -a -f %{_infodir}/mu-guile.info.gz ]; then
	/sbin/install-info --delete \
	--info-dir=%{_infodir} %{_infodir}/mu-guile.info.gz || :
fi


%files
%defattr(-,root,root)
%{_bindir}/mu
%{_mandir}/man1/*
%{_mandir}/man5/*
%{_datadir}/mu/*

%{_emacs_sitelispdir}/mu4e
%{_emacs_sitelispdir}/mu4e/*.elc
%{_emacs_sitelispdir}/mu4e/*.el
%{_infodir}/mu4e.info.gz

%files gtk
%{_bindir}/mug

%files guile
%{_libdir}/libguile-mu.*
%{_datadir}/guile/site/2.0/mu/*
%{_datadir}/guile/site/2.0/mu.scm
%{_infodir}/mu-guile.info.gz

%changelog
* Wed Feb 12 2014 Santiago Saavedra <ssaavedra@gpul.org> - 0.9.9.5-1
- Create first SPEC.
