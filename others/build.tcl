#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {[file exists yass.gpr] == 0} {
   puts {This script must be run in the directory where yass.gpr file is}
   return
}

# Set the target for the compilation. If no arguments, use system default
if {$argc == 0} {
   if {$tcl_platform(os) == "Linux"} {
      set target x86_64-linux-gnu
   } else {
      set target x86_64-windows
   }
} else {
   set target [lindex $argv 0]
}

# Check if correct target was set
if {$target != "x86_64-linux-gnu" && $target != "x86_64-windows"} {
   puts {Invalid compilation target. Allowed options are x86_64-linux-gnu and x86_64-windows}
   return
}

# Set the directory where the release will be created
if {$target == "x86_64-linux-gnu"} {
   set releasedir usr
} else {
   set releasedir release
}

exec gprclean -P yass.gpr --target=$target >@stdout
exec gprbuild -p -P yass.gpr -XMode=release --target=$target >@stdout
puts -nonewline {Copying files and directories ... }
if {$target == "x86_64-linux-gnu"} {
   file mkdir $releasedir/share/doc/yass
   file mkdir $releasedir/share/metainfo/
   file copy bin $releasedir/
   file copy COPYING $releasedir/share/doc/yass/
   file copy README.md $releasedir/share/doc/yass/
   file copy CONTRIBUTING.md $releasedir/share/doc/yass/
   file copy others/yass.appdata.xml $releasedir/share/metainfo
} else {
   file mkdir $releasedir
   file copy [file join bin yass.exe] $releasedir
   file copy COPYING $releasedir
   file copy README.md $releasedir
   file copy CONTRIBUTING.md $releasedir
}
puts {done}
exec gprclean -P yass.gpr --target=$target >@stdout
