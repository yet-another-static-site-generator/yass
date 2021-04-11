#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" ${1+"$@"}

if {![file exists yass.gpr]} {
   puts {This script must be run in the directory where yass.gpr file is}
   return
}

exec gprbuild -p -P tests/driver/test_driver.gpr >@stdout
cd [file join tests driver]
set result [exec [file join "[pwd]" test_runner]]
puts $result
if {[string first FAILED $result] > -1 || [string first CRASHED $result] > -1} {
   exit 1;
}
