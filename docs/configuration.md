-- layout: default
-- title: Configuring Project
-- backlink: tags.html
-- backtext: Tags
-- nextlink: pages.html
-- nexttext: Page Config
-- indexlink: true
-- details: []
-- details: General Informations
-- details: Project configuration
-- details: Project wide (global) tags
-- detailslink: []
-- detailslink: general
-- detailslink: configuration
-- detailslink: tags
## <a name="general"></a>General Informations

Whole project configuration is stored in the file *site.cfg* in main site project
directory. This file have standard configuration syntax `[Key] = [Value]`. Each
pair must be in one line, neither `Key` nor `Value` can't contains new line.
Each line which starts with `#` is comment, ignored by the program. All
settings are case-sensitive.

<a href="#top">^ Top</a>

## <a name="configuration"></a>Project configuration

Possible settings for project:

- LayoutDirectory - the path (related to project directory path) where are
  stored files with pages layouts. Standard value for this is setting is
  `_layouts`.
- OutputDirectory - the path (related to project directory path) where
  generated site will be stored. Standard values for this setting is
  `_output`.
- ModulesDirectory - the path (related to project directory path) where
  the program modules will be stored. Standard values for this setting is
  `_modules`.
- ExcludedFiles - the list of excluded files and directories from the list
  of sources used to generating the site. All paths must be relative to the
  project directory. If you exclude directory, it whole content will be
  excluded too. Layouts, modules and output directories are excluded by
  default. Default value for this is empty list.
- ServerEnabled - can have true of false value, case-insensitive. Did command
  `server` should start web server too or not. Default value is `true` (start
  the web server).
- ServerPort - any number from 1 to 65535. Port on which the web server will
  be listening. Please remember that using ports below 1025 number require
  root privileges. Default value is `8888`
- MonitorInterval - any positive number. How often (in seconds) the program
  should check for this site changes when working in server mode and
  regenerate the site if needed. Default value is `5.0`.
- BaseURL - Base URL of the site. It is needed mostly for creating sitemap,
  but you can use it as a normal the site tag. If your site will be available
  at `https://mysite.com/blog` then this will be your BaseURL. Default value is
  `http://localhost:8888`.
- EnableSitemap - Did the program should create sitemap when creating the site.
  Possible values are true or false (case-insensitive). Default value is
  `true` (create sitemap).

<a href="#top">^ Top</a>

## <a name="tags"></a>Project wide (global) tags

Project configuration file can contains project wide (global) tags, which can
be used on each page. For more informations about global tags, please see
section [Templates Tags](tags.html).

<a href="#top">^ Top</a>
