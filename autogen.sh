#!/bin/sh
# Run this to generate all the initial makefiles, etc.

echo "*** meson build setup"

test -f mu/mu.cc || {
    echo "*** Run this script from the top-level mu source directory"
    exit 1
}
BUILDDIR=build

command -v meson 2> /dev/null
if [ $? != 0 ]; then
    echo "*** No meson found, please install it ***"
    exit 1
fi

# we could remove build/ but let's avoid rm -rf risks...
if test -d ${BUILDDIR}; then
    meson --reconfigure ${BUILDDIR} $@
else
    meson ${BUILDDIR} $@
fi

echo "*** Now run 'ninja -C ${BUILDDIR}' to build mu"
echo "*** Or check the Makefile for some useful targets"
