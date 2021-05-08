-- layout: default
-- title: ChangeLog
-- backlink: extending.html
-- backtext: Extending
-- indexlink: true
-- details: []
-- details: Unreleased
-- details: 2.0
-- details: 1.1
-- details: 1.0
-- details: 0.8
-- details: 0.7
-- details: 0.6
-- details: 0.5
-- details: 0.4
-- details: 0.3
-- details: 0.2
-- details: 0.1
-- detailslink: []
-- detailslink: 3.0
-- detailslink: 2.0
-- detailslink: 1.1
-- detailslink: 1.0
-- detailslink: 0.8
-- detailslink: 0.7
-- detailslink: 0.6
-- detailslink: 0.5
-- detailslink: 0.4
-- detailslink: 0.3
-- detailslink: 0.2
-- detailslink: 0.1
## <a name="3.0"></a>[Unreleased]
### Changed
- Updated README.md
- Updated contributing guide

### Fixed
- Saving and showing crash data on Windows
- Grammar and typos in README.md and in the documentation

## <a name="2.0"></a>[2.0] - 2019-11-17

### Added
- Coloring the program messages
- Interactive creation of new projects

### Changed
- Create command to createnow

### Fixed
- Typos in README.md and CONTRIBUTING.md
- Server crash on lack of output directory
- Adding Atom link to pages

## <a name="1.1"></a>[1.1] - 2019-10-23
### Added
- Contributing guide
- Code documentation
- Added Atom link to the default page template
- Language configuration option to the site

### Changed
- Updated README.md

### Fixed
- Typos in list of changes
- Crash on blocket socket when starts the program in server mode

<a href="#top">^ Top</a>

## <a name="1.0"></a>[1.0] - 2019-03-25
### Added
- Better check for spawn web browser by web server

### Changed
- Updated README.md

### Fixed
- Editing simple tags by modules
- Reading Atom feed entries amount from site configuration file
- Names of settings for server intervals in fresh site configuration files

## <a name="0.8"></a>[0.8] - 2019-03-17
### Added
- Time to updated date in sitemaps
- Self link to the Atom feed
- Author info to the Atom feed and it entries
- Summary tag to the Atom feed entries
- Content tag to the Atom feed entries

### Changed
- Updated README.md

<a href="#top">^ Top</a>

## <a name="0.7"></a>[0.7] - 2019-03-12
### Added
- API to read/edit template tags values via the program modules

### Changed
- Updated README.md

### Fixed
- Crash on adding site-wide global tags to the page
- Crash when browser command is not set

<a href="#top">^ Top</a>

## <a name="0.6"></a>[0.6] - 2019-03-08
### Added
- Better validation of page tags values
- Ability to set absolute paths for directories
- Option to set tags separators
- Link to Atom entry as it content
- Link element to Atom entries
- Option to set separator for comments in markdown files
- Command to create empty markdown files
- Directory listing for the web server
- Option to stop server on error occurred during creating the site
- Option to start selected web browser on start web server
- Monitoring and auto reloading site configuration file on changes in it

### Changed
- Added release date to version command
- Don't create Atom feed by default
- Updated README.md

### Fixed
- Showing README.md file
- Read modules directory setting
- Saving Atom entry id
- Info about server web address

<a href="#top">^ Top</a>

## <a name="0.5"></a>[0.5] - 2019-02-23

### Added
- Time stamp to server messages
- Default setting for excluded files and directories in site configuration
  files
- Generating sitemaps
- Generating Atom feed

### Changed
- New documentation look
- Updated README.md
- Don't allow spaces in tags names

<a href="#top">^ Top</a>

## <a name="0.4"></a>[0.4] - 2019-02-12

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
- Crash on invalid data in site configuration files

<a href="#top">^ Top</a>

## <a name="0.3"></a>[0.3] -  2019-02-03

### Added
- Better info about lack of layout file
- Better info about errors in templates files
- Server monitor changes in layout files too
- Composite tags

### Changed
- Updated README.md

### Fixed
- Server crash on missing file during site generation
- Server crash on error in template file
- Crash on comment in markdown file

<a href="#top">^ Top</a>

## <a name="0.2"></a>[0.2] - 2019-01-24

### Added
- ChangeLog file
- Support for a boolean and a numeric values in template tags
- Documentation

### Changed
- Replaced the templates system with Ada Web Server templates
- Handling server shutdown
- Updated README.md
- Name of content variable from `Contents` to `Content`

<a href="#top">^ Top</a>

## <a name="0.1"></a>[0.1] - 2019-01-16
Initial release

<a href="#top">^ Top</a>
