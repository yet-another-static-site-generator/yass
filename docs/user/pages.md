-- layout: default
-- title: Configuring Pages
-- backlink: configuration.html
-- backtext: Site Config
-- nextlink: extending.html
-- nexttext: Extending
-- indexlink: true
-- details: []
-- details: General Informations
-- details: Page configuration
-- detailslink: []
-- detailslink: general
-- detailslink: configuration
## <a name="general"></a>General informations
At this moment, all configuration for each page is inside files with page
content. They have syntax: `[CommentMark] [Key]: [Value]`. Default setting
for the `[CommendMark]` is `--` but you can change it in `site.cfg` file.

- Same as site settings, each pair must be in one line. Neither `Key` nor
  `Value` can't contains new line.
- Settings can be added in any place of file (at beginning, between text, at
  end, etc) just they must start with new line.
- All settings are case-sensitive.

<a href="#top">^ Top</a>

## <a name="configuration"></a>Page configuration

Available settings for each page:

If you have enabled creation of sitemap in the project config file, you can
set some sitemap parameters too. All sitemap parameter are optional. More
informations about how sitemap parameters works, you can find at
[Sitemaps protocol](https://www.sitemaps.org/protocol.html#xmlTagDefinitions)
page. All sitemap settings are optional.

- priority - The priority of this URL relative to other URLs on your site.
  Valid values range from `0.0` to `1.0`. This value does not affect how your
  pages are compared to pages on other sites—it only lets the search engines
  know which pages you deem most important for the crawlers.
- changefreq - How frequently the page is likely to change, value can be:
  `always`, `hourly`, `daily`, `weekly`, `monthly`, `yearly` or `never`.
- insitemap - optional setting. Did page should be included in sitemap. If you
  want to exclude this page from the sitemap, set it to `insitemap: false`.
  Default value is `true`.

If you enabled creation of Atom feed in the project config file, you can some
Atom feed parameters too. How much and how it works, depends on selected source
of the Atom feed. All of this parameters can be used as a normal page tags too.
More informations about how Atom feeds parameters works, you can find at
[Introduction to Atom](https://validator.w3.org/feed/docs/atom.html)

If you selected source of the Atom feed as `tags`:

- title - Title which will be used as the Atom title entry. Required setting if
  creating Atom feed is enabled.
- updated - Time stamp when page (and Atom feed entry) last time was updated.
  Must be in [RFC3339](http://www.faqs.org/rfcs/rfc3339.html) time style. This
  parameter is optional. If you not set it, the program will use time when file
  html (created from this markdown file) was last time updated.
- author - The name of the author of the page. This parameter is optional.
- authoremail - The email address of the author of the page. This parameter is
  optional.
- summary - Short summary for this page. This parameter is optional.
- content - Content of the page in Atom feed. If you not set it, then the
  program will load here markdown version of the page content. This parameter
  is optional.

Example of markdown file with Atom feed settings for Atom feed source as
`tags`:

    -- layout: default
    -- title: awesome text about something
    Here is super awesome text.

If you selected source of the Atom feed as `[filename]` and this markdown file
will be used as a source for the Atom feed:

- title - Composite tag (if you want to have more that one entry in the Atom
  feed) which will be used as the Atom entries titles. Required setting if
  creating Atom feed is enabled.
- id - name or id of the HTML element which will be entry point for each Atom
  entry. It will be added to the page address in Atom entries. Required
  setting for creating Atom feed from one file.
- updated - Time stamp when selected Atom feed entry was last time updated.
  Must be in [RFC3339](http://www.faqs.org/rfcs/rfc3339.html) time style.
  Required for each entry if creating Atom feed from one file is enabled.
- author - The name of the author of selected Atom feed entry. This is
  optional parameter for each entry.
- authoremail - The email address of the author of selected Atom feed entry.
  This is optional parameter for each entry.
- summary - Short summary for selected Atom feed entry. This is optional
  parameter for each entry.
- content - Content of selected Atom feed entry. If you not set it, then the
  program will load here id of selected Atom feed entry. This parameter is
  optional.

Example of markdown file `test.md` with Atom feed settings for Atom feed
source as `test.md`:

    -- layout: news
    -- title: []
    -- id: []
    -- updated: []
    -- news: []
    -- title: third news
    -- id: latest
    -- updated: 2019-02-23T10:05:00Z
    -- authoremail: jondoe@example.com
    -- author: Jon Doe
    -- news: This is the latest news.
    -- id: third
    -- updated: 2019-02-23T10:02:00Z
    -- news: This is the third news.
    -- title: second news
    -- id: second
    -- updated: 2019-02-23T10:00:00Z
    -- authoremail: jondoe@example.com
    -- news: This is the second news.
    -- title: first news
    -- id: first
    -- updated: 2019-02-23T09:59:00Z
    -- author: Jon Doe
    -- news: This is the first news.

And use [TABLE statement](http://docs.adacore.com/aws-docs/templates_parser/template_statements.html#table-statement)
inside `news.html` layout to show that news:

    <!DOCTYPE html>
    <html lang="en">
      <head>
         <meta charset="UTF-8">
         <title>News</title>
      </head>
      <body>
         @@TABLE@@
            <h2>{%title%}<a name="{%id%}"></a></h2>
            <p>{%NO_LETTER:updated%}</p>
            <p>{%news%}</p>
         @@END_TABLE@@
      </body>
    </html>

One parameter is always required:

- layout - the name of file (without extension) which will be used as template
  for the page. This must be existing file from the project layouts directory.

<a href="#top">^ Top</a>
