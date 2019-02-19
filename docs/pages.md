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
content. They have syntax: `-- [Key]: [Value]`.

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

- priority - The priority of this URL relative to other URLs on your site,
  value between 0.0 and 1.0. Valid values range from `0.0` to `1.0`. This
  value does not affect how your pages are compared to pages on other sites—it
  only lets the search engines know which pages you deem most important for the
  crawlers.
- changefreq - How frequently the page is likely to change, value can be:
  `always`, `hourly`, `daily`, `weekly`, `monthly`, `yearly` or `never`.
- insitemap - optional setting. Did page should be included in sitemap. If you
  want to exclude this page from the sitemap, set it to `insitemap: false`.
  Default value is `true`.
- layout - the name of file (without extension) which will be used as template
  for the page. This must be existing file from the project layouts directory.
  It is the only required setting.

<a href="#top">^ Top</a>
