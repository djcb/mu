** --muhome
Use a non-default directory to store and read the database, write the logs, etc.
By default, *mu* uses the XDG Base Directory Specification (e.g. on GNU/Linux this
defaults to _~/.cache/mu_ and _~/.config/mu_). Earlier versions of *mu* defaulted to
_~/.mu_, which now requires *--muhome=~/.mu*.

The environment variable *MUHOME* can be used as an alternative to *--muhome*. The
latter has precedence.

# Local Variables:
# mode: org
# End:
