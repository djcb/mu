#!/bin/sh
# Run this to generate all the initial makefiles, etc.

test -f mu/mu.cc || {
    echo "*** Run this script from the top-level mu source directory"
    exit 1
}

command -V autoreconf > /dev/null
if [[ $? != 0 ]]; then
    echo "*** No autoreconf found, please install it ***"
    exit 1
fi

rm -f config.cache
rm -rf autom4te.cache

autoreconf --force --install --verbose || exit $?

if test -z "$*"; then
    echo "# Configuring without parameters"
else
   echo "# Configure with parameters $*"
fi

./configure --config-cache $@
