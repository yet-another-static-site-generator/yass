#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {[file exists yass.gpr] == 0} {
   puts {This script must be run in the directory where yass.gpr file is}
   return
}

# directory where the release will be created
set releasedir usr

exec gprclean -P yass.gpr >@stdout
exec gprbuild -p -P yass.gpr -XMode=release >@stdout
puts -nonewline {Copying files and directories ... }
file mkdir $releasedir/share/doc/yass
file mkdir $releasedir/share/metainfo/
file copy bin $releasedir/
file copy COPYING $releasedir/share/doc/yass/
file copy README.md $releasedir/share/doc/yass/
file copy CONTRIBUTING.md $releasedir/share/doc/yass/
file copy others/yass.appdata.xml $releasedir/share/metainfo
puts {done}
exec gprclean -P yass.gpr >@stdout
