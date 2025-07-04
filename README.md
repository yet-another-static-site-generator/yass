YASS: Yet Another Static Site (Generator)

YASS is a static site generator written in Ada. It is *headless*
application (no user interface). The program documentation is included in
distribution.

## Features

* Support almost infinite amount of custom tags in HTML templates
* Separated tags for whole site and each page
* Fast
* Can be extended with modules written in any script/programming language
* Generating sitemaps
* Generating Atom feeds
* Auto reconfigure server when configuration file was changed

## Downloading YASS

The easiest way to get YASS is by downloading the [latest release](https://github.com/yet-another-static-site-generator/yass/releases/latest) for your operating system.  For example, if you have windows, then download ` yass-windows_x86_64.zip`.

## Build from sources

### Using alire

The easiest way to build YASS is via [Alire](https://alire.ada.dev/).  If you don't
 have alire already, you can get it from the main website or auto-install it via
 [Getada.dev](https://www.getada.dev/).

To build and install it, simply run `alr install yass` and yass will be
added to alire's binary folder (default is `$HOME/.alire/bin`).

If you'd rather just download and build manually, navigate to a folder where you
 wish to build YASS and issue the following commands:
 1. `alr get yass`
 2. `cd yass*`
 3. `alr update && alr build`
 YASS will now be in the project's `./bin` directory.

This should pull all of the dependencies in, ie AWS and libcmark.

### Using gprbuild

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

Navigate to the main directory (where this file is) to compile:

* The easiest way to compile program is to use Gnat Studio included in
  GNAT. Select *yass.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`.

### Build unit tests

**Note:** Unit tests are currently being migrated to aunit and alire so this may
not work as expected.

Navigate to `tests/driver` directory from the main directory (where this
file is):

* From console: type `gprbuild -P test_driver.gpr`


## Running the program

To see all available options, type `yass help`.

### Running unit tests

From the main directory go to `test/driver` directory and type `./test_runner`.

### Testing versions

There are available testing versions of the program. You can find them
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

To generate code documentation, you need [ROBODoc](https://rfsber.home.xs4all.nl/Robo/)
and [Tcl](https://tcl.tk). In main program directory enter: `others/generatedocs.tcl`.
For more information, please look [here](https://github.com/thindil/roboada#generatedocspy).
This version of the script have set all default settings for the YASS code.

## Contributing to the project
For detailed information about contributing (bugs reporting,
ideas propositions, code conduct, etc), see [CONTRIBUTING.md](CONTRIBUTING.md)

## Licenses

* YASS is released under GNU GPL v3 license.

* Libcmark library distributed with the program is released under
[a few Open Sources licenses](https://github.com/commonmark/cmark/blob/master/COPYING)

  https://github.com/commonmark/cmark

## TODO

* More unit tests
* Formally verify the project with SPARK
* Your propositions?

----

As usual, I probably forgot about something important here :)

Bartek thindil Jasicki & A.J. Ianozi
