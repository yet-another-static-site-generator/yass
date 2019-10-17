#!/usr/bin/env sh


releasedir=usr

gprbuild -p yass.gpr -XMode=release
mkdir -p $releasedir/share
mkdir -p $releasedir/bin
cp bin/yass $releasedir/bin
cp README.md $releasedir/share
cp COPYING $releasedir/share
cp CONTRIBUTING.md $releasedir/share
