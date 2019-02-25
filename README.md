YASS - Yet Another Static Site (Generator)

As name says, it is static site generator written in Ada. It is *headless*
application (no user interface). The program documentation is included in
distribution.

**Note:** This version of README.md is for the development version of the program.
It may (and probably will be, especially now, on beginning of the development)
differ from the released versions. For the released version please refer to
files included in distributed packages.

## Features

* Support almost infinite amount of custom tags in HTML templates (depends
  on available RAM)
* Separated tags for whole site and each page
* Fast
* Can be extended with modules written in any script/programming language
* Generating sitemaps
* Generating Atom feeds
* Freshly created, unpolished, have a few bugs :D

## Build from sources

To build you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

* gprbuild - it is included in GNAT and should be available in most Linux
  distributions too.

* libcmark - should be available in every Linux distribution, if not, you
  can download source code from:

  https://github.com/commonmark/cmark

* Ada Web Server (AWS) - if you use GNAT from AdaCore it is included in
  package. In other situations, you may need to download it from:

  https://www.adacore.com/download/more

  or

  https://github.com/AdaCore/aws

* XmlAda - if you use GNAT from AdaCore it is included in package. In other
  situation, you may need to download it from:

  https://www.adacore.com/download/more

  or

  https://github.com/AdaCore/xmlad://github.com/AdaCore/xmlada 

Navigate to the main directory(where this file is) to compile:

* Easiest way to compile program is use Gnat Programming Studio included in
  GNAT. Just run GPS, select *yass.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`.

If you want to be able to print content of README.md file to terminal (by
`readme` program command), copy file *README.md* to `bin` directory.

**Note:** If you want to move the program around, compile it in release mode. In
debug mode the program may have problems with finding all dependencies.

## Running program

### Linux

To see all available options, type in console `./yass help` in directory where
binary file is. It work that same way for downloaded AppImage version of
program. More informations about using AppImage files you can find here:

https://docs.appimage.org/user-guide/run-appimages.html

If you want to run the program from other directory, you should set the
environment variable `YASSDIR` to your current directory. Example:
`export YASSDIR=$(pwd)`. You don't need to set it manually when you use
AppImage version of the program.

## Roadmap

### 0.6
- Better web server
- More configuration options

### 0.7
- Better scripting support

### 0.8
- Better generating of atom feed and sitemap

### 0.9
- Some formal verification of code

### 1.0
- Bugfixes, code cleanup and optimization

### Always

- Your proposition(s)

## Licenses

- Libcmark library distributed with AppImage version of the program is released
under [few Open Sources licenses](https://github.com/commonmark/cmark/blob/master/COPYING)

https://github.com/commonmark/cmark

----

As usual, I probably forgot about something important here :)

Bartek thindil Jasicki
