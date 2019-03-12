-- layout: default
-- title: Extending Program
-- backlink: pages.html
-- backtext: Page Config
-- nextlink: changes.html
-- nexttext: Changelog
-- indexlink: true
-- details: []
-- details: General Informations
-- details: Adding modules to the site project
-- details: Communication between YASS and modules
-- details: API informations
-- details: Example module
-- detailslink: []
-- detailslink: general
-- detailslink: adding
-- detailslink: communication
-- detailslink: api
-- detailslink: example
## <a name="general"></a>General informations

YASS can be extended with any script or other program which can be run on your
machine. This mean, for example Bash, Perl, Python scripts or any compiled
program, anything what can be run from terminal. Second requirement is that
extension (called *module*) must have ability to read environment variables,
read standard input and write to standard output, because in that ways YASS and
module communicate. Modules are per site not assigned to the program, thus
every site can have own set of modules to run.

<a href="#top">^ Top</a>

## <a name="adding"></a>Adding modules to the site project

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

<a href="#top">^ Top</a>

## <a name="communication"></a>Communication between YASS and modules

There are two ways for communication between the program and modules.
- By environment variables: Before running modules in *pre* hook YASS
  is setting environment variable `YASSFILE` with full path to currently
  modified file. Before running module in *post* hook YASS is setting
  environment variable `YASSFILE` with full path to result file.
  Additionally, modules have access to all other environment variables
  currently set. For example to `YASSDIR` or `APPDIR` if you use AppImage
  version of the program.
- By pipes: When the program starts the module, the program grabs input and
  output of the module. Here is available very simple API to manipulate then
  template tags. To use it, module should just send proper command to default
  output and listen to the program's response on default input.

<a href="#top">^ Top</a>

## <a name="api"></a>API informations

- gettag [tagname] - this API command sent to the program, returns value of
  `tagname` tag. If selected tag is the simple tag, then the program
  immediately value of selected tag. If selected tag is composite tag then
  the program first send amount of the values of the tag and then each value
  for this tag. If tag with selected name does not exists, the program will
  inform the module about it.
- edittag [tagname] (tagindex) [tagvalue] - this API command sent to the
  program, sets new value for selected tag. If selected tag is the simple tag,
  then variable `tagindex` is not needed (example: `edittag name newname`).
  When selected tag is the composite tag, then variable `tagindex` is required.
  Index range for the composite tags starts from 1. If YASS properly modify
  the value of the tag, it will send to module answer `Success`. If there
  will be any problem with modification, then the program will send the
  information what was wrong.
- During *start* and *end* *hooks* there are available only global tags from
  `site.cfg` file. During *pre* and *post* *hooks* you can manipulate with page
  tags either.
- Whole command must be one line command, it can't contains new line symbols.

<a href="#top">^ Top</a>

## <a name="example"></a>Example module

This is very small example of module, which show name of currently processed
file, then show value of tag "Name", then set second value of tag "news" and
at the end show all values of the tag "news".

    #!/bin/sh

    echo "Now processing: $YASSFILE"
    # get value for tag "Name"
    echo "gettag Name"
    # read value of the tag "Name"
    read name
    # print value of the tag
    echo "$name"
    # set value of tag "Name" to "NewName"
    echo "edittag Name NewName"
    # read result of the operation
    echo "Result:$result"
    # set second value of tag "news" to "NewSite2"
    echo "edittag news 2 NewSite2"
    # get result of operation and print it
    read result of the operation
    echo "Result:$result"
    # get all values for composite tag "news"
    echo "gettag news"
    # read how many values this tag have
    read amount
    # read and print values of the tag
    for (( i=1; i<=$amount; i++ ))
    do
      read newvalue
      echo "$newvalue"
    done


Save it as `showname.sh` file, give it permissions to run `chmod 744
showname.sh` and put it in `modules/pre` directory in selected site project.
Additionally, you been need to add or to `site.cfg` or to markdown file the
composite tag `news` with at least 2 values. For example in `site.cfg`:

    news = []
    news = first news
    news = second news
    news = last news

<a href="#top">^ Top</a>
