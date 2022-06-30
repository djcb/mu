#!/bin/sh

infodir=$1
infofile=$2

# Meson post-install script to update info metadata
install-info --info-dir ${MESON_INSTALL_DESTDIR_PREFIX}/${infodir} \
	     ${MESON_INSTALL_DESTDIR_PREFIX}/${infodir}/${infofile}

gzip --force ${MESON_INSTALL_DESTDIR_PREFIX}/${infodir}/${infofile}
