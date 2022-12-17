* COMMON OPTIONS

** -d, --debug
makes mu generate extra debug information, useful for debugging the program
itself. By default, debug information goes to the log file, ~/.cache/mu/mu.log.
It can safely be deleted when mu is not running. When running with --debug
option, the log file can grow rather quickly. See the note on logging below.

** -q, --quiet
causes mu not to output informational messages and progress information to
standard output, but only to the log file. Error messages will still be sent to
standard error. Note that mu index is much faster with --quiet, so it is
recommended you use this option when using mu from scripts etc.

** --log-stderr
causes mu to not output log messages to standard error, in addition to sending
them to the log file.

** --nocolor
do not use ANSI colors. The environment variable ~NO_COLOR~ can be used as an
alternative to ~--nocolor~.

** -V, --version
prints mu version and copyright information.

** -h, --help
lists the various command line options.

# Local Variables:
# mode: org
# End:
