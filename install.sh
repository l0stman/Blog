#!/bin/sh -x

INSTDIR=/var/lib/hunchentoot/.sbcl
SITDIR=$INSTDIR/site
SYSDIR=$INSTDIR/systems

install -d $SITDIR $SYSDIR
rsync -tr * $SITDIR
cd $SYSDIR
if ! [ -L blog.asd ]; then
    ln -s $SITDIR/blog.asd
fi
