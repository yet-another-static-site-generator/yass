-- layout: default
-- title: Detailed Information About Program Options
-- backlink: quickstart.html
-- backtext: Quick Start
-- nextlink: tags.html
-- nexttext: Tags
-- indexlink: true
## Notes
* If you want to run the program from other directory, you should set the
  environment variable `YASSDIR` to your current directory. Example:
  `export YASSDIR=$(pwd)`.
* If you use AppImage version of program, replace `yass` in examples with
  name of AppImage file (if not changed, this will be `yass-x86_64.AppImage`).

## Creating new site project

To create new site project type in the directory where the file `yass` is
`./yass create [ProjectName]` where `[ProjectName]` is the name of your site
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

## Building existing project

To build any existing site project, type in directory where the file `yass`
is `./yass build [ProjectName]` where `[ProjectName]` is name of your site
project. It will convert any not excluded file with extension `.md` to
HTML file, apply layout to it and move with all others, not excluded files
to the output directory (by default it is *_output* directory in `[ProjectName]`
directory). This command preserve structure of directories and files in
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
directory):

    index.html
    images
      |-newimage.png

## Server mode

To run the program in server mode, type in the directory where file `yass`
is `./yass server [ProjectName]` where `[ProjectName]` is name of your site
project. If you enabled the web server in `site.cfg` this will start it
at *http://localhost:[port]/* (where *[port]* is port number set in `site.cfg`,
by default it is *8888*) to which you can point your web browser to preview
the site. Additionally, the program will be monitoring every *X* seconds (this
is set in `site.cfg` file too, default every 5 seconds) your site project
directory for changes to files. If any file will be changed during server work,
it will regenerate it. If you add any new file, it will be added to the site
output directory. If you change any layout file from layouts directory all
HTML files which use this layout (directly, not as included) will be updated
too.

### Limitations
- At this moment, when you browsing your project, you must enter valid address
  to the file. Going just to *http://localhost:[port]/* will not works, you must
  enter *http://localhost:[port]/index.html*
- Monitoring does not delete files. If you delete one, the program will not
  detect it. You must stop, remove old file manually, then restart the server.

## Help command

This command gives only short information about all available program's
commands. To run it, type in directory where the file `yass` is `./yass help`.

## Version command

This command gives only short information about the program version. To run
it, type in directory where the file `yass` is `./yass version`.

## License command

This command gives only short information about the program license. To run
it, type in directory where the file `yass` is `./yass license`.

## Readme command

This command prints in terminal content of README.md file. To run it, type in
directory where the file `yass` is `./yass readme`.
