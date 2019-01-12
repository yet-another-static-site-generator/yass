YASS - Yet Another Static Site (Generator)

As name says, it is static site generator written in Ada. At this moment
available is only almost working prototype.

## Build from sources

To build you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

* gprbuild - it is included in GNAT and should be available in most Linux
  distributions too.

* libcmark - should be available in every Linux distribution, if not, you
  can download source code from:

  https://github.com/commonmark/cmark

Navigate to the main directory(where this file is) to compile:

* Easiest way to compile game is use Gnat Programming Studio included in GNAT.
  Just run GPS, select *yass.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`.

## Running program

### Linux

To see all available options, type in console `./yass help` in directory where
binary file is.

## Roadmap

### 0.1

- Support for Windows
- Replace templates system with AWS templates

### Later

- Scripting with Lua
- TUI
- GUI

Bartek thindil Jasicki
