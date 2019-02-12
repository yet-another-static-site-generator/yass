# Changelog
All notable changes to this project will be documented in this file.

## [0.4] - 2019-02-12

### Added
- Help/info to site configuration files
- Help/info to default empty index.md files
- Option to print license and README.md in terminal
- Option to enable or disable web server
- Option to set web server port
- Option to set interval for the program to monitor changes in site
- Ability to extend the program with modules per site

### Changed
- Updated README.md

### Fixed
- Crash on invalid data in site config files

## [0.3] - 2019-02-03

### Added
- Better info about lack of layout file
- Better info about errors in templates files
- Server monitor changes in layout files too
- Composite tags

### Changed
- Updated README.md
- Regenerate only needed files not whole site

### Fixed
- Server crash on missing file during site generation
- Server crash on error in template file
- Crash on comment in markdown file

## [0.2] - 2019-01-24

### Added
- ChangeLog file
- Support for boolean and numeric values in tags
- Documentation

### Changed
- Replaced templates system with Ada Web Server templates
- Handling server shutdown
- Updated README.md
- Name of content variable from `Contents` to `Content`

## [0.1] - 2019-01-16
Initial release
