#!/bin/sh

# Very simple startup script. The only thing which it do is setting LD_LIBRARY_PATH environment variable. Use this script only if you use downloaded compiled version of YASS. In other way just run yass executable file.

LD_LIBRARY_PATH=. ./yass "$@"
