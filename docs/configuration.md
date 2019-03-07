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

- LayoutDirectory - the path (absolute or related to project directory path)
  where are stored files with pages layouts. Standard value for this is setting
  is `_layouts`.
- OutputDirectory - the path (absolute related to project directory path) where
  generated site will be stored. Standard values for this setting is
  `_output`.
- ModulesDirectory - the path (absolute or related to project directory path)
  where the program modules will be stored. Standard values for this setting is
  `_modules`.
- ExcludedFiles - the list of excluded files and directories from the list
  of sources used to generating the site. All paths must be relative to the
  project directory. If you exclude directory, it whole content will be
  excluded too. Layouts, modules and output directories are excluded by
  default. Default value for this is empty list.
- ServerEnabled - can have true or false value, case-insensitive. Did command
  `server` should start web server too or not. Default value is `true` (start
  the web server).
- ServerPort - any number from 1 to 65535. Port on which the web server will
  be listening. Please remember that using ports below 1025 number require
  root privileges. Default value is `8888`
- StopServerOnError - can have true or false value, case-insensitive. Did
  command `server` should terminate if error occured, during creating the site.
  Default value is `false` (don't terminate on error).
- BrowserCommand - Full path to the command which will be used to start the web
  browser with index.html page of the site. String `%s` will be replaced by
  server URL. If this setting is `none`, the web browser will be not started,
  same as when the web server is disabled. Default value is `none`.
- MonitorInterval - any positive number. How often (in seconds) the program
  should check for this site changes when working in server mode and
  regenerate the site if needed. Default value is `5.0`.
- BaseURL - Base URL of the site. It is needed mostly for creating sitemap and
  Atom feed, but you can use it as a normal the site tag. If your site will be
  available at `https://mysite.com/blog` then this will be your BaseURL.
  Default value is `http://localhost:8888`.
- EnableSitemap - Did the program should create sitemap when creating the site.
  Possible values are true or false (case-insensitive). Default value is
  `true` (create sitemap).
- AtomFeedSource - Source which will be used for creating Atom feed of the
  site. Possible values are: `none` - don't create Atom feed, `tags` - create
  Atom entries from proper tags in .md files, `[filename]` - the path (related to
  the project directory path) to markdown file which will be used as a source of
  Atom feed (must have proper tags set inside). For more informations about
  Atom feed tags, please see section [Configuring Pages](pages.html). Default
  value is `none`.
- AtomFeedAmount - Number of entries in the Atom feed (if it will be created,
  please see setting above).
- StartTagSeparator - any string which will be used as start mark for the
  templates tags. You may want to change it, if you want to use templates from
  other static site generators. Default value is `{%`.
- EndTagSeparator - any string which will be used as end mark for the templates
  tags. You may want to change it, if you want to use templates from other
  static site generators. Default value is `%}`.
- MarkdownComment - any string which will be used as a start mark for comments
  in parsed markdown files. Default value is `--`

<a href="#top">^ Top</a>

## <a name="tags"></a>Project wide (global) tags

Project configuration file can contains project wide (global) tags, which can
be used on each page. For more informations about global tags, please see
section [Templates Tags](tags.html).

<a href="#top">^ Top</a>
