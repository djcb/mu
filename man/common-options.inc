* COMMON OPTIONS

** -d, --debug
Makes *mu* generate extra debug information, useful for debugging the program
itself. Debug information goes to the standard logging location; see
{{{man-link(mu,1)}}}.

** -q, --quiet
Causes *mu* not to output informational messages and progress information to
standard output, but only to the log file. Error messages will still be sent to
standard error. Note that *mu index* is much faster with *--quiet*, so it is
recommended you use this option when using *mu* from scripts etc.

** --log-stderr
Causes *mu* to not output log messages to standard error, in addition to sending
them to the standard logging location.

** --nocolor
Do not use ANSI colors. The environment variable *NO_COLOR* can be used as an
alternative to *--nocolor*.

** -V, --version
Prints *mu* version and copyright information.

** -h, --help
Lists the various command line options.

# Local Variables:
# mode: org
# End:
