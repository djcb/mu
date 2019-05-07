
# Table of Contents

1.  [Spec file for bulding mu on Fedora](#orgd8f5f49)
2.  [Instalation](#org94e6a4a)
    1.  [Adding repo](#org4f2c563)
    2.  [Installation of packages](#org7bacbe9)


<a id="orgd8f5f49"></a>

# Spec file for bulding mu on Fedora

An attempt to create spec file for building packages for mu and emacs-mu4e.
This spec file has been tested to create packages on Fedora.
Unfortunately, it conflicts with mu-editor ([GitHub](https://github.com/mu-editor/mu) and [Home Page](https://codewith.mu)) as using the same program name.

Packages can be found at <https://copr.fedorainfracloud.org/coprs/bojov/mu-fedora>


<a id="org94e6a4a"></a>

# Instalation

The following packages are included:

-   mu-mail-search
-   mu-mail-search-guile
-   emacs-mu4e


<a id="org4f2c563"></a>

## Adding repo

dnf copr enable bojov/mu-fedora


<a id="org7bacbe9"></a>

## Installation of packages

dnf install mu-mail-search mu-mail-search-guile emacs-mu4e
