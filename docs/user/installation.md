-- layout: default
-- title: Installation
-- nextlink: quickstart.html
-- nexttext: Quick Start
-- indexlink: true
-- details: []
-- details: Requirements
-- details: Build from sources
-- detailslink: []
-- detailslink: requirements
-- detailslink: buildprogram


## <a name="downloading"></a>Downloading

The easiest way to get YASS is by downloading the [latest release](https://github.com/yet-another-static-site-generator/yass/releases/latest) for your operating system.  For example, if you have windows, then download ` yass-windows_x86_64.zip`.

## <a name="requirements"></a>Requirements

To run the program or build it from the sources you need *libcmark* library
which should be available in most Linux distributions. If not, you can
download it source code from [GitHub](https://github.com/commonmark/cmark).
Please refer to it documentation on how to compile it.

<a href="#top">^ Top</a>

## <a name="buildprogram"></a>Build from sources

### Using Alire

The easiest way to build YASS is via [Alire](https://alire.ada.dev/).  If don't
 have alire already, you can get it from the main website or auto-install it via
 [Getada.dev](https://www.getada.dev/).

To build and install it, simply run `alr install yass` and yass will automatically be 
added to alire's binary folder (default is `$HOME/.alire/bin`).

If you'd rather just download and build manually, navigate to a folder where you
 wish to build YASS and issue the following commands:
 1. `alr get yass`
 2. `cd yass*`
 3. `alr update && alr build`
 YASS will now be in the project's `./bin` directory.

This should automatically pull all of the dependencies in, ie AWS and libcmark.

### Using gprbuild

#### Preparations

To build YASS from the source you will need few things:

* Ada compiler: GCC with enabled Ada support or (best option) GNAT from:
  [AdaCore](https://www.adacore.com/download/)

* gprbuild: it is included in GNAT and should be available in most Linux
  distributions too.

* Ada Web Server (AWS): if you use GNAT from AdaCore it is included in
  package. In other situations, you may need to download it from:
  [AdaCore](https://www.adacore.com/download/more) or
  [GitHub](https://github.com/AdaCore/aws)

#### Build program

Navigate to the main directory to compile:

* The easiest way to compile the program is use Gnat Programming Studio included
  in GNAT. Just run GPS, select *yass.gpr* as a project file and select option
  `Build All`.

* If you prefer using console: in main source code directory type `gprbuild`
  for debug mode build or for release mode: `gprbuild -XMode=release`. If you
  have installed [Bob](https://github.com/thindil/bob) you can type `bob debug`
  for build in debug mode or `bob release` to prepare release for the program.

If you want to be able to print content of README.md file to terminal (by
`readme` program command), copy file README.md to `bin` directory.

**Note:** If you want to move the program around, compile it in release mode. In
debug mode the program may have problems with finding all dependencies.

### Build unit tests

Navigate to `tests/driver` directory from the main directory (where this
file is):

* From console: type `gprbuild -P test_driver.gpr`

Or if you have [Bob](https://github.com/thindil/bob) installed, type
`bob tests`.

<a href="#top">^ Top</a>
