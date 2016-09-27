# mup #

mup is a perl interface to [mu](http://www.djcbsoftware.nl/code/mu/)
([GitHub](https://github.com/djcb/mu)), a Maildir indexing and
search system that also implements the core functionality needed
by pretty much any MUA.

mup replicates the API described in the
[mu-server(1)](http://manpages.ubuntu.com/manpages/precise/man1/mu-server.1.html) man page in a pleasingly Perly style.  It is licensed under the
[ISC license](http://en.wikipedia.org/ISC_licnse), a simplified
variant of the [BSD license](http://en.wikipedia.org/BSD_licenses).

mup works in the same way the elisp code in mu4e does: it forks
a `mu-server` process and communicates with it.  I use the
[Data::SExpression](http://search.cpan.org/~nelhage/Data-SExpression-0.41/lib/Data/SExpression.pm) CPAN module to deal with `mu-server`'s LISPy result syntax,
which we transform into the obvious hashrefian results our callers crave
(they've got electrolytes).

## Tests ##

I use standard perl testing stuff (`Test::More`).  The tests all
operate on a temporary Maildir/mu index created by `t/lib.pm`.  If you
are interested in hacking on or understanding the tests you should
first look at `t/lib.pm` to see how the temporary setup is created and
torn down.  All tests should have

    use t::lib;

in them somewhere near the top.  This is all that is necessary to make
sure the code in the test does not e.g. hose down your actual
~/Maildir and/or ~/.mu.
