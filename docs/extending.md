-- layout: default
-- title: Extending Program
-- backlink: pages.html
-- backtext: Page Config
-- nextlink: changes.html
-- nexttext: Changelog
-- indexlink: true
## General informations

YASS can be extended with any script or other program which can be run on your
machine. This mean, for example Bash, Perl, Python scripts or any compiled
program, anything what can be run from terminal. Second requirement is that
extension (called *module*) must have ability to read environment variables,
because at this moment, this is only one way to communicate between YASS and
module. Modules are per site not assigned to the program, thus every site can
have own set of modules to run.

## Adding modules to the site project

After creating a site project, you will have directory `_modules` (with default
setting) inside site project directory. This directory have structure:

    [ProjectName]
      |-_modules
         |-end
         |-post
         |-pre
         |-start

Subdirectories inside `_modules` directory are places where all modules should
be put. They names are that same like the program *hooks* for running modules
thus you should not change names for this subdirectories.

- start: put here modules which will be run at beginning of work for commands
  `build` and `server`. This modules will be executed only once during build or
  regenerate the site. For command `server` this happens only after start the
  server.
- pre: put here modules which will be run every time when the program will be
  preparing to manipulate any file, like copying or generating HTML page from
  markdown file.
- post: put here modules which will be run every time when the program will end
  manipulating any file, like copying or generating HTML page from markdown
  file.
- end: put here modules which will be run after finishing files manipulation.
  For command `server` it will be run after every site regeneration.

Thus, if you have module `test.sh` and you want to run it every time when the
program finish manipulating file, you must put it in path `modules/post`:

    [ProjectName]
      |-_modules
         |-end
         |-post
            |-test.sh
         |-pre
         |-start

YASS checking for modules only this four subdirectories, don't go recursively
deeper. Thus, modules can be not only one executable file, but even whole
directories. In that situation, you will need to provide any "entry point": one
executable to run that complicated module.

## Communication between YASS and modules

At this moment here is only one way communication between the program and
modules, by environment variables. Before running modules in *pre* hook YASS
is setting environment variable `YASSFILE` with full path to currently
modified file. Before running module in *post* hook YASS is setting environment
variable `YASSFILE` with full path to result file. Additionally, modules have
access to all other environment variables currently set. For example to
`YASSDIR` or `APPDIR` if you use AppImage version of the program.

## Example module

This is very small example of module, which show name of currently processed
file.

    #!/bin/sh

    echo "Now processing: $YASSFILE"

Save it as `showname.sh` file, give it permissions to run `chmod 744
showname.sh` and put it in `modules/pre` directory in selected site project.
