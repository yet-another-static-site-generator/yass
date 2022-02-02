YASS: Yet Another Static Site (Generator)

As name says, it is static site generator written in Ada. It is *headless*
application (no user interface). The program documentation is included in
distribution.

**INFO:** This project is no longer maintained. Feel free to clone it and take
care about it.

## Features

* Support almost infinite amount of custom tags in HTML templates (depends
  on available RAM)
* Separated tags for whole site and each page
* Fast
* Can be extended with modules written in any script/programming language
* Generating sitemaps
* Generating Atom feeds
* Auto reconfigure server when configuration file was changed

## Build from sources

### Docker way

You can use Docker images `adabuild` and `adabuildwin64` from the project
[Docker Ada](https://www.laeran.pl/repositories/dockerada). They contain all libraries
and compiler needed to build the program.

To build the program for Linux, download `adabuild` image and type in console:

`docker run --rm -v [path to source code]:/app ghcr.io/thindil/adabuild /bin/bash -c "cd /app && gprbuild -p -P yass.gpr -XMode=release"`

To build the program for Windows 64-bit, download `adabuildwin64` image and type in console:

`docker run --rm -v [path to source code]:/app ghcr.io/thindil/adabuildwin64 /bin/bash -c "cd /app && gprbuild -p -P yass.gpr -XMode=release --target=x86_64-windows"`

### Classic way

To build you need:

* compiler: GCC with enabled Ada support or GNAT from:

  https://www.adacore.com/download/

* gprbuild: it is included in GNAT from AdaCore and should be available in most
  Linux distributions too.

* libcmark: should be available in every Linux distribution, if not, you
  can download source code from:

  https://github.com/commonmark/cmark

* Ada Web Server (AWS): if you use GNAT from AdaCore it is included in
  package. In other situations, you may need to download it from:

  https://www.adacore.com/download/more

  or

  https://github.com/AdaCore/aws

* XmlAda: if you use GNAT from AdaCore it is included in package. In other
  situation, you may need to download it from:

  https://www.adacore.com/download/more

  or

  https://github.com/AdaCore/xmlada

Navigate to the main directory(where this file is) to compile:

* The easiest way to compile program is use Gnat Studio included in
  GNAT. Just run GPS, select *yass.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`. If you
  have installed [Bob](https://www.laeran.pl/repositories/bob) you can type `bob debug`
  for build in debug mode or `bob release` to prepare release for the program.

If you want to be able to print content of README.md file to terminal (by
`readme` program command), copy file *README.md* to `bin` directory.

**Note:** If you want to move the program around, compile it in release mode. In
debug mode the program may have problems with finding all dependencies.

### Build unit tests

Navigate to `tests/driver` directory from the main directory (where this
file is):

* From console: type `gprbuild -P test_driver.gpr`

Or if you have [Bob](https://www.laeran.pl/repositories/bob) installed, type
`bob tests`.

## Running the program

### Linux

To see all available options, type in console `./yass help` in directory where
binary file is. It works that same way for downloaded AppImage version of
program. More information about using AppImage files you can find here:

https://docs.appimage.org/user-guide/run-appimages.html

If you want to run the program from other directory, you should set the
environment variable `YASSDIR` to your current directory. Example:
`export YASSDIR=$(pwd)`. You don't need to set it manually when you use
AppImage version of the program.

### Windows

To see all available options, type in console `yass.exe help` in the directory
where binary file is. If you want to run the program from other directory, you
should set the environment variable `YASSDIR` to your current directory.
Example: `set YASSDIR="C:\yass"`

### Running unit tests

From the main directory (where this file is) go to `test/driver` directory
and type in console `./test_runner`. If you have [Bob](https://www.laeran.pl/repositories/bob)
installed, you can type `bob runtests`.

### Testing versions

Here are available testing versions of the program. You can find them
in [GitHub Actions](https://github.com/yet-another-static-site-generator/yass/actions/workflows/ada.yml).
Just select option from the list of results to see Artifacts list.
To use them, first you must download normal release. Then, for Linux: inside
directory where the program is, type `./yass-x86_64.AppImage --appimage-extract`
to extract whole program to directory *squashfs-root*. And then move files
from the archive to the proper location. To run that version, enter
*squashfs-root* directory and type in console `./AppRun`. For Windows:
unzip files (replace existing) to the proper location where the program is installed.

* yass-development-windows.tar contains Windows 64-bit version of the program.

* yass-development-linux.tar contains Linux 64-bit version of the program.

Size is a file's size after unpacking. You will download it compressed with
Zip.

## Generating code documentation

To generate (or regenerate) code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/)
and [Tcl](https://tcl.tk) scripting language. If you have them, in main program
directory (where this file is) enter terminal command: `others/generatedocs.tcl`.
For more information about this script, please look [here](https://github.com/thindil/roboada#generatedocspy).
This version of script have set all default settings for the YASS code. If you
have [Bob](https://www.laeran.pl/repositories//bob) installed, you can type
`bob docs`.

## Contributing to the project
For detailed information about contributing to the project (bugs reporting,
ideas propositions, code conduct, etc), see [CONTRIBUTING.md](CONTRIBUTING.md)

## Licenses

* Yass is released under GNU GPL v3 license.

* Libcmark library distributed with the program is released under
[a few Open Sources licenses](https://github.com/commonmark/cmark/blob/master/COPYING)

https://github.com/commonmark/cmark

## TODO (someday or if someone wants to contribute)

* More unit tests
* Formally verify the project with SPARK
* Your propositions?

----

As usual, I probably forgot about something important here :)

Bartek thindil Jasicki
