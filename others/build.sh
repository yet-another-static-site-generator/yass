#!/usr/bin/env sh


releasedir=usr

gprbuild -p yass.gpr -XMode=release
mkdir -p $releasedir/share/doc/yass/
mkdir -p $releasedir/bin
cp bin/yass $releasedir/bin
cp README.md $releasedir/share/doc/yass
cp COPYING $releasedir/share/doc/yass
cp CONTRIBUTING.md $releasedir/share/doc/yass
