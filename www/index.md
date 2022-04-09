---
layout: default
permalink: /code/mu/
---

# Welcome to mu!

<img src="mu.jpg" align="right" margin="10px"/> With the *enormous* amounts of e-mail many people
gather and the importance of e-mail messages in our daily work-flow, it is very important to be able
to quickly deal with all that - in particular, to instantly find that one important e-mail you need
right now.

For that, *mu* was created. *mu* is a tool for dealing with e-mail messages stored in the
[Maildir](http://en.wikipedia.org/wiki/Maildir)-format, on Unix-like systems. *mu*'s main purpose is
to help you to find the messages you need, quickly; in addition, it allows you to view messages,
extract attachments, create new maildirs, ... See the [mu cheatsheet](cheatsheet.html) for some
examples. Mu's source code is available [in github](https://github.com/djcb/mu), and there is the
[mu-discuss](http://groups.google.com/group/mu-discuss) mailing list.

*mu* includes an emacs-based e-mail client (`mu4e`), a simple GUI (`mug`) and bindings for the
Guile/Scheme programming language.

## Features

- fast indexing for [Maildir](http://en.wikipedia.org/wiki/Maildir), Maildir+ and Maildir-on-VFAT
- search for messages based on the sender, receiver, subject, date-range,
size, priority, words in message, flags (signed, encrypted, new, replied,
has-attachment,...), message-id, maildir, tags, attachment (name,
mime-type, text) and more
- support for encrypted and signed messages
- command-line tools for indexing, searching, viewing, adding/removing
messages, extracting attachments, exporting/searching address lists,
creating maildirs, ...
- accent/case normalization - so *angstrom* matches *Ångström*
- can be integrated with other e-mail clients such as
[mutt](http://www.mutt.org/) and
[Wanderlust](http://www.emacswiki.org/emacs/WanderLust).
- [mu4e](mu4e.html), an emacs-based e-mail client based on `mu` (see screenshot).
- [mu-guile](mu-guile.html):
[guile 2.0](http://www.gnu.org/software/guile/) bindings that
allow for scripting, advanced processing of your data, and doing
all kinds of statistics
- fully documented (man pages, info pages)

## News

### 2021-07-27: mu/mu4e 1.6 is available

A new release is available;  [release notes](https://github.com/djcb/mu/releases/tag/1.6) and
grab the [tarball](https://github.com/djcb/mu/releases/download/1.6.10/mu-1.6.10.tar.xz) (latest 1.6.x release).

### 2020-04-18: mu/mu4e 1.4 is available

A new release is available;  [release notes](https://github.com/djcb/mu/releases/tag/1.4) and
grab the [tarball](https://github.com/djcb/mu/releases/download/1.4.15/mu-1.4.15.tar.xz) (latest 1.4.x release).


### 2019-04-07: mu/mu4e 1.2 is available

A new release is available; see the [release notes](https://github.com/djcb/mu/releases/tag/1.2) and
grab the [tarball](https://github.com/djcb/mu/releases/download/1.2/mu-1.2.0.tar.xz).


### 2018-02-03: mu/mu4e 1.0 is available

After a decade of development, mu 1.0 is available. Read
[NEWS](https://github.com/djcb/mu/blob/v1.0/NEWS.org) with all the details.

### 2016-12-05: mu/mu4e 0.9.18 is available

mu 0.9.18 offers a number of improvements across the board. For
example, people with huge maildirs can use a special "lazy-checking"
mode to speed up indexing; it's now possible to view rich-text message
in an embedded webkit-view, and the release adds support for org-mode
9.x. There also many small fixes and tweaks in mu4e, all based on
user-feedback.

For all the details,
see: [NEWS.org](https://github.com/djcb/mu/blob/0.9.18/NEWS.org).

Get it from the [Release page](https://github.com/djcb/mu/releases).

### 2016-01-21: mu/mu4e 0.9.16 is here, and it is our latest stable release!

#### Better behaviour and context handling
- Context Handling just got smart:  new ‘mu4e-context’ defines and switches between various contexts, which are groups of settings. This may be used for instance to easily configure and switch between multiple accounts.
- Improved behaviour in html and messages marks: ability to toggle between html and text display of messages & better management of messages marked as read or unread. 

#### User Interface improvements
- Numerous improvements in threads view and mailing lists management
- Fancy characters can now be properly used as well as special customizations for message views

#### Faster Indexing and message management
- Indexing & caching optimizations

You can grab the tarball directly
[from Github](https://github.com/djcb/mu-releases) or wait a bit to
get it through your distribution channels (details may vary from one
distribution to another).

None of this would be possible without a team of dedicated
individuals: Adam Sampson, Ævar Arnfjörð Bjarmason, Bar Shirtcliff,
Charles-H. Schulz, Clément Pit--Claudel, Damien Cassou, Declan Qian,
Dima Kogan, Dirk-Jan C. Binnema, Foivos, Foivos S. Zakkak, Hinrik Örn
Sigurðsson, jeroen tiebout, JJ Asghar, Jonas Bernoulli, Jun Hao,
Martin Yrjölä, Maximilian Matthé, Piotr Trojanek, prsarv, Thierry
Volpiatto, Titus von der Malsburg (and of course all people who
reported issues, provided suggestions etc.)

We hope you will enjoy this release as much as we do. Happy Hacking!

-- The mu/mu4e Team

## Old News

- 2015-09-24: After almost 6 months, a new release of mu/mu4e. We are
happy to announce mu and mu4e 0.9.9.13! have just been
released. The following key features and improvements have been
added:

* Change the way the headers are displayed and sorted
* Fancy characters now enabled distinctively both for marks and
headers
* Composing a message is now possible in a separate frame
* Ability to display the subject of a thread only on top of it for
enhanced clarity
* Lots of bugs squashed, updates to the documentation (BDDB), as
well as embedding the News file inside mu4e itself.


- 2013-03-30: released [mu-0.9.9.5](http://code.google.com/p/mu0/downloads/detail?name%3Dmu-0.9.9.5.tar.gz); full with new features and bug
fixes – see the download link for some of the details. Many
thanks to all who contributed!
- 2012-10-14: released [mu-0.9.9](http://code.google.com/p/mu0/downloads/detail?name%3Dmu-0.9.9.tar.gz); a new barrage of fixes and
improvements – check the link and [NEWS](https://github.com/djcb/mu/blob/master/NEWS). Also, note the
[mu4e-manual](http://code.google.com/p/mu0/downloads/detail?name%3Dmu4e-manual-0.9.9.pdf) (PDF).
- 2012-07-01: released [mu-0.9.8.5](http://code.google.com/p/mu0/downloads/detail?name%3Dmu-0.9.8.5.tar.gz); more fixes, improvements (see
the link).
- 2012-05-08: released
[mu-0.9.8.4](http://code.google.com/p/mu0/downloads/detail?name%3Dmu-0.9.8.4.tar.gz)
with even more improvements (the link has all the details)
- 2012-04-06: released
[mu-0.9.8.3](http://code.google.com/p/mu0/downloads/detail?name%3Dmu-0.9.8.3.tar.gz),
with many improvements, fixes. See the link for details. *NOTE*:
existing `mu` and `mu4e` users are recommended to execute `mu
index --rebuild` after installation.
- 2012-03-11: released
[mu-0.9.8.2](http://code.google.com/p/mu0/downloads/detail?name=mu-0.9.8.2.tar.gz),
with a number of fixes and improvements, see the link for the
details.
- 2012-02-17: released
[mu-0.9.8.1](http://code.google.com/p/mu0/downloads/detail?name%3Dmu-0.9.8.1.tar.gz),
which has a number of improvements to the 0.9.8 release: add mark
as read/unread, colorize cited message parts, better handling of
text-based message parts, documentation fixes, documentation
updates and a few fixes here and there
- 2012-02-09: moved the mu source code repository
[to Github](https://github.com/djcb/mu).
- 2012-01-31: finally,
[mu-0.9.8](http://mu0.googlecode.com/files/mu-0.9.8.tar.gz) is
available. It comes with an emacs-based e-mail client,
[mu4e](file:mu4e.html), and much improved
[guile bindings](file:mu-guile.html). Furthermore, It adds
search for attachment mime type and search inside any text part
of a message, more tests, improvements in many parts of the code.
- 2011-09-03: mu 0.9.7 is now available; compared to the -pre
version there are a few small changes; the most important one is
a fix specifically for running mu on MacOS.

- [Old news](file:old-news.org)

## Development & download

<a href="mu4e-splitview.png" border="0"><img src="mu4e-splitview-small.png" align="right" margin="10px"/></a>

Some Linux-distributions already provide pre-built mu packages; if
there's no packagage for your distribution, or if you want the
latest release, you can [download mu source packages](http://code.google.com/p/mu0/downloads/list) from Google
Code. In case you find a bug, or have a feature requests, please
use the [issue tracker](https://github.com/djcb/mu/issues).

If you'd like to work with the mu source code, you can find it [in Github](https://github.com/djcb/mu);
also, see the notes on [HACKING](https://github.com/djcb/mu/blob/master/HACKING) the mu source code.

There's also a [mailing list](http://groups.google.com/group/mu-discuss).

## License & Copyright

*mu* was designed and implemented by Dirk-Jan C. Binnema, and is Free
Software, licensed under the GNU GPLv3
