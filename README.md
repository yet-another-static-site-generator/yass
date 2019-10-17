YASS - Yet Another Static Site (Generator)

As name says, it is static site generator written in Ada. It is *headless*
application (no user interface). The program documentation is included in
distribution.

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
  for debug mode build or for release mode: `gprbuild -XMode=release`. If you
  have installed *Bob* you can type `bob debug` for build in debug mode or
  `bob release` to prepare release for the program.

If you want to be able to print content of README.md file to terminal (by
`readme` program command), copy file *README.md* to `bin` directory.

**Note:** If you want to move the program around, compile it in release mode. In
debug mode the program may have problems with finding all dependencies.

### Build unit tests

Navigate to `tests/driver` directory from the main directory (where this
file is):

* From console: type `gprbuild -P test_driver.gpr`

Or if you have *Bob* installed, type `bob tests`.

## Running the program

### Linux

To see all available options, type in console `./yass help` in directory where
binary file is. It work that same way for downloaded AppImage version of
program. More informations about using AppImage files you can find here:

https://docs.appimage.org/user-guide/run-appimages.html

If you want to run the program from other directory, you should set the
environment variable `YASSDIR` to your current directory. Example:
`export YASSDIR=$(pwd)`. You don't need to set it manually when you use
AppImage version of the program.

### Running unit tests

From the main directory (where this file is) go to `test/driver` directory
and type in console `./test_runner`. If you have *Bob* installed, you can type
`bob runtests`.

## Generating code documentation

To generate (or regenerate) code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/)
If you have it, in main program directory (where this file is) enter terminal
command: `others/generatedocs.py`. For more information about this script,
please look [here](https://github.com/thindil/roboada#generatedocspy). This
version of script have set all default settings for Hunter code. If you have
*Bob* installed, you can type `bob docs`.

## Contributing to the project
For detailed informations about contributing to the project (bugs reporting,
ideas propositions, code conduct, etc), see [CONTRIBUTING.md](CONTRIBUTING.md)

## Licenses

- Yass is released under GNU GPL v3 license.

- Libcmark library distributed with AppImage version of the program is released
under [few Open Sources licenses](https://github.com/commonmark/cmark/blob/master/COPYING)

https://github.com/commonmark/cmark

## TODO (someday or if someone want to contribute)

- Windows version of the program
- More unit tests
- Your propositions?

----

As usual, I probably forgot about something important here :)

Bartek thindil Jasicki
