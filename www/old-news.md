---
layout: default
permalink: code/mu/old-news.html
---

# Old news

- 2011-07-31: mu *0.9.7-pre* is now available with a number of interesting
new features and fixes, many based on user suggestions. `mu` now supports
/mail threading/ based on the [JWZ-algorithm](http://www.jwz.org/doc/threading.html); output is now automatically
converted to the user-locale; `mu view` can output separators between
messages for easier processing, support for X-Label-tags, and last but not
least, `mu` now has bindings for the [Guile](http://www.gnu.org/s/guile/) (Scheme) programming language -
there is a new toy (`toys/muile`) that allows you to inspect messages and
do all kinds of statistics - see the [README](https://gitorious.org/mu/mu/blobs/master/toys/muile/README) for more information.

- 2011-06-02: after quite a bit of testing, *0.9.6* has been promoted to be
the next release -- forget about the 'bèta'. Development continues for
the next release.

- 2011-05-28: *mu-0.9.6* (bèta). A lot of internal changes, but also quite
some new features, for example:
- wild-card searching for most fields: mu find 'car*'
- search for message with certain attachments with 'attach:/a:': mu find
'attach:resume*'
- color for `mu find`, `mu cfind`, `mu extract` and `mu view`
Everything is documented in the man-pages, and there are examples in the [[file:cheatsheet.org][mu
cheatsheet]].

- 2011-04-25: *mu-0.9.5* a small, but important, bugfix in maildir-detection,
some small optimizations.

- 2011-04-12: *mu 0.9.4* released - adds the `cfind` command, to find
contacts (name + e-mail); add `flag:unread` which is a synonym for
`flag:new OR NOT flag:seen`. Updates to the documentation and some internal
updates. This is a *bèta-version*.

- 2011-02-13: *mu 0.9.3*; fixes a bunch of minor issues in 0.9.2; updated the
web page with pages for [mu cheatsheet](file:mug.org][mug]] (the experimental UI) and the [[file:cheatsheet.org).

- 2011-02-02: *mu 0.9.2* released, which adds support for matching on message
size, and various new output format. See [NEWS](http://gitorious.org/mu/mu/blobs/master/NEWS) for all the user-visible
changes, also from older releases.


- [2010-12-05] *mu version 0.9.1* released; fixes a couple of issues users
found with a missing icon, the unit-tests.
- [2010-12-04] *mu version 0.9* released. Compared to the bèta-release, there
were a number of improvements to the documentation and the unit
tests. Pre-processing queries is a little bit smarter now, making matching
e-mail address more eager. Experimental support for Fedora-14.
- [2010-11-27] *mu version 0.9-beta* released. New features: searching is now
accent-insensitive; you can now search for message priority (`prio:`),
time-interval (`date:`) and message flags (`flag:`). Also, you can now store
('bookmark') often-used queries. To top it off, there is a simple graphical
UI now, called `mug`. Documentation has been update, and all known bugs have
been fixed.
- [2010-10-30] *mu version 0.8* released, with only some small cosmetic
updates compared to 0.8-beta. Hurray!
- [2010-10-23] *mu version 0.8-beta* released. The new version brings `mu
extract` for getting the attachments from messages, improved searching
(matching is a bit more 'greedy'), updated and extended documentation,
including the `mu-easy` manpage with simple examples. All known
non-enhancement bugs were fixed.
- [2010-02-27] *mu version 0.7* released. Compared to the beta version, there
are few changes. The maildir-matching syntax now contains a starting `/`, so
`~/Maildir/foo/bar/(cur|new)/msg` can be matched with `m:/foo/bar`. The
top-level maildir can be matched with `m:/`. Apart from that, there are so
small cosmetic fixes and documentation updates.
- [2010-02-11] *mu version 0.7-beta* released. A lot of changes:
- Automatic database scheme version check, notifies users when an
upgrade is needed
- Adds the `mu view` command, to view mail message files
- Removes the 10K match limit
- Support for unattended upgrades - that is, the database can
automatically be upgraded (`--autoupgrade`). Also, the log file is
automatically cleaned when it gets too big (unless you use
`--nocleanup`)
- Search for a certain Maildir using the `maildir:`,`m:` search
prefixes. For example, you can find all messages located in
`~/Maildir/foo/bar/(cur|new)/msg` with `m:foo/bar`. This replaces the
search for path/p in 0.6
- Fixes for reported issues #17 and #18
- A test suite with a growing number of unit tests
- Updated documentation
- Many internal refactoring and other changes
This version has been
tagged as `v0.7-beta` in repository, and must be considered a code-complete
preview of the upcoming release 0.7. Please report any problems you encounter
with it.
