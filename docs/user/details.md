-- layout: default
-- title: Detailed Information About Program Options
-- backlink: quickstart.html
-- backtext: Quick Start
-- nextlink: tags.html
-- nexttext: Tags
-- indexlink: true
-- details: []
-- details: Notes
-- details: Creating new site project
-- details: Building existing project
-- details: Server mode
-- details: Help command
-- details: Version command
-- details: License command
-- details: Readme command
-- details: Create empty markdown file
-- detailslink: []
-- detailslink: notes
-- detailslink: create
-- detailslink: build
-- detailslink: server
-- detailslink: help
-- detailslink: version
-- detailslink: license
-- detailslink: readme
-- detailslink: createfile
## <a name="notes"></a>Notes
* If you want to run the program from other directory, you should set the
  environment variable `YASSDIR` to your current directory. Example:
  `export YASSDIR=$(pwd)`.
* If you use AppImage version of program, replace `yass` in examples with
  name of AppImage file (if not changed, this will be `yass-x86_64.AppImage`).

<a href="#top">^ Top</a>

## <a name="create"></a>Creating new site project

To create new site project type in the directory where the file `yass` is
`./yass createnow [ProjectName]` for fast create of new project with default
settings, or `./yass create [ProjectName]` for creation of new project based
on your answers to few questions. `[ProjectName]` is the name of your site
project. It will be the name of directory too. This command create the
directory with the content:

    [ProjectName]
      |-_layouts
         |-default.html
      |-_output
      |-_modules
         |-end
         |-post
         |-pre
         |-start
      |-index.md
      |-site.cfg

* Directory *_layouts* contains Ada Web Server templates used to generate the
  site.
* Directory *_output* is the directory where generated site will be stored.
* Directory *_modules* is the directory where the program modules will be
  stored.
* File *index.md* is empty Markdown file which will be later main page of the
  site.
* File *site.cfg* is configuration file of project.

<a href="#top">^ Top</a>

## <a name="build"></a>Building existing project

To build any existing site project, type in directory where the file `yass`
is `./yass build [ProjectName]` where `[ProjectName]` is name of your site
project. It will convert any not excluded file with extension `.md` to
HTML file, apply layout to it and move with all others, not excluded files
to the output directory (by default it is *_output* directory in `[ProjectName]`
directory). Additonally, if you have enabled generating sitemaps, it will
create one with all pages and file `robots.txt` to inform crawlers about the
sitemap. This command preserve structure of directories and files in
your project. Thus if you create the directory structure like this:

    [ProjectName]
      |-_layouts
         |-default.html
      |-_output
      |-_modules
         |-end
         |-post
         |-pre
         |-start
      |-index.md
      |-site.cfg
      |-images
         |-newimage.png

Your new site will be have that directory structure (inside the *_output*
directory). Files `sitemap.xml` and `robots.txt` will be added only when
you will be have enabled option to create sitemap. File `atom.xml` will
be added only when you will be have enabled option to create Atom feed:

    index.html
    sitemap.xml
    robots.txt
    atom.xml
    images
      |-newimage.png

<a href="#top">^ Top</a>

## <a name="server"></a>Server mode

To run the program in server mode, type in the directory where file `yass`
is `./yass server [ProjectName]` where `[ProjectName]` is name of your site
project. If you enabled the web server in `site.cfg` this will start it
at *http://localhost:[port]/* (where *[port]* is port number set in `site.cfg`,
by default it is *8888*) to which you can point your web browser to preview
the site. If you enable it too, after the web server start, the program will
open `index.html` page in selected web browser. Additionally, the program will
be monitoring every *X* seconds (this is set in `site.cfg` file too, default
every *5* seconds) your site project directory for changes to files. If any
file will be changed during server work, it will regenerate it. If you add any
new file, it will be added to the site output directory. If you change any
layout file from layouts directory all HTML files which use this layout
(directly, not as included) will be updated too. If you have enabled creating
sitemaps, it will be upgraded or created too if needed, same with file
`robots.txt`. If you have enabled creating atom feed, it will be upgrade or
create `atom.xml` file if needed. If you want, you can set this command to
terminate on any error occurence, during monitoring the site. Additionally,
you can customize how directory listing looks, by editing `directory.html`
file in layouts directory. More informations about available tags, you can
find in [AWS API documentation]
(https://docs.adacore.com/aws-docs/aws/apiref.html#aws-services-directory).

### Limitations
- Monitoring does not delete files. If you delete one, the program will not
  detect it. You must stop, remove old file manually, then restart the server.
- If you delete page which was in the sitemap, you will be need to edit the
  sitemap manually or delete it completely with file `robots.txt` to
  regenerate list of the site pages with `build` command.
- Same with Atom feed, if you delete page which was in feed, you will need to
  edit `atom.xml` file or delete it to regenerate it with `build` command.
- If you edit global tag (in `site.cfg` file) you will be need to stop the
  program and rebuild whole site with `build` command, because server can
  monitor only settings changes (it will not rebuild `.md` files).

<a href="#top">^ Top</a>

## <a name="help"></a>Help command

This command gives only short information about all available program's
commands. To run it, type in directory where the file `yass` is `./yass help`.

<a href="#top">^ Top</a>

## <a name="version"></a>Version command

This command gives only short information about the program version and it
release date. To run it, type in directory where the file `yass` is
`./yass version`.

<a href="#top">^ Top</a>

## <a name="license"></a>License command

This command gives only short information about the program license. To run
it, type in directory where the file `yass` is `./yass license`.

<a href="#top">^ Top</a>

## <a name="readme"></a>Readme command

This command prints in terminal content of README.md file. To run it, type in
directory where the file `yass` is `./yass readme`.

<a href="#top">^ Top</a>

## <a name="createfile"></a>Create empty markdown file

If you want to create a new empty (only with comments) markdown file in your
site project, type in directory where the file `yass` is
`./yass [path][filename].md`. You may omit file extension, it will be added
automatically by the program.

<a href="#top">^ Top</a>
