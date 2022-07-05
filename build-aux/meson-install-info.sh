#!/bin/sh

infodir=$1
infofile=$2

# Meson post-install script to update info metadata
# If DESTDIR is set, do _not_ install-info, since it's only a temporary
# install
if test -z "${DESTDIR}"; then
    install-info --info-dir ${MESON_INSTALL_DESTDIR_PREFIX}/${infodir} \
		 ${MESON_INSTALL_DESTDIR_PREFIX}/${infodir}/${infofile}
fi

gzip --force ${MESON_INSTALL_DESTDIR_PREFIX}/${infodir}/${infofile}
