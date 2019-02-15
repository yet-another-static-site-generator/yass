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

At this moment is available only one setting for each page:

- layout - the name of file (without extension) which will be used as template
  for the page. This must be existing file from the project layouts directory.

<a href="#top">^ Top</a>
