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
    echo "*** 'meson' not found, please install it ***"
    exit 1
fi

# we could remove build/ but let's avoid rm -rf risks...
if test -d ${BUILDDIR}; then
    meson setup --reconfigure ${BUILDDIR} $@ || exit 1
else
    meson setup ${BUILDDIR} $@ || exit 1
fi

echo "*** Now run either 'ninja -C ${BUILDDIR}' or 'make' to build mu"
echo "*** Check the Makefile for other useful targets"
