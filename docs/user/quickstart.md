-- layout: default
-- title: Quick Start Guide
-- backlink: installation.html
-- backtext: Installation
-- nextlink: details.html
-- nexttext: Details
-- indexlink: true
-- details: []
-- details: Notes
-- details: Quick start
-- detailslink: []
-- detailslink: notes
-- detailslink: quickstart
## <a name="notes"></a>Notes
* All commands should be executed in this same directory where file `yass` is.
* If you want to run the program from other directory, you should set the
  environment variable `YASSDIR` to your current directory. Example:
  `export YASSDIR=$(pwd)`.
* If you use AppImage version of program, replace `yass` in examples with
  name of AppImage file (if not changed, this will be `yass-x86_64.AppImage`).
* We assume that you have installed all program requirements, which you can
  find in [installation](installation.html) chapter.

<a href="#top">^ Top</a>

## <a name="quickstart"></a>Quick start

1. To create NewSite project, type in console `./yass createnow NewSite`. This
   will create skeletal project with default configuration in directory
   `NewSite`. If you prefer to have more control over creation of new project,
   use command `./yass create NewSite` which allow you to set some settings
   for the new project.
2. Inside newly created directory `NewSite` you can find file *index.md*, which
   later will be main page of the site and *site.cfg* which contains all basic
   settings of the site. Add some markdown text to *index.md* file or some new
   files to directory `NewSite`.
3. When you finish editing project, type in console `./yass build NewSite`
   to build your new site. Effect of this command will be stored (by default)
   in directory `_output`.
4. If you want, you can run program in server mode, by typing in console
   `./yass server NewSite`. It will start simple web server, which allow you,
   to preview site in browser. Additionally, it will be monitoring any changes
   to files inside project. If any file will be modified, it will be
   regenerated.

<a href="#top">^ Top</a>
