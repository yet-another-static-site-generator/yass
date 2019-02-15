-- layout: default
-- title: Template Tags
-- backlink: details.html
-- backtext: Details
-- nextlink: configuration.html
-- nexttext: Site Config
-- indexlink: true
-- details: []
-- details: Types of tags
-- details: Using tags inside templates files
-- detailslink: []
-- detailslink: types
-- detailslink: using
## <a name="types"></a>Types of tags

Here are two types of tags:

### Global tags

They are defined for whole site in *site.cfg* file. This file have standard
config file syntax: `[Key] = [Value]`. For example:

    Name = Yet Another Static Site (Generator) Documentation

- Each pair must be in one line. Neither `Key` nor `Value` can't contains new
  line.
- You can add as many as you want tags to project.

### Local (page) tags

They are defined directly in pages content files (markdown files with `.md`
extension). They have syntax: `-- [Key]: [Value]`. For example:

    -- title: Template Tags

- Same as global tags, each pair must be in one line. Neither `Key` nor `Value`
  can't contains new line.
- Same as global tags, you can add as many as you want tags to project.
- Tags can be added in any place of a file (at beginning, between text, at end,
  etc) just they must start with new line. The program can't detect tags inside
  text.
- Whole text from a markdown file is stored in tag `Content`.

### Values of tags

Both type of tags can have string, numeric or boolean values. For string and
numeric value just enter value. String value example:

    -- title: title of page

Numeric value example:

    -- paragraphnumber: 12

For boolean values, enter as value *true* or *false* (both case-insensitive).
Example:

    -- havetitle: true

### Composite tags

Composite tags are tags with a few values (like arrays, tuples or vectors in
programming). They can be declared as global or per page. At this moment they
can have only string values. Before they can be set, they must be initialized
by giving them value `[]`. For example, global composite tags in *site.cfg*
file:

    summary = []

For selected page, like normal local (page) tags:

    -- paragraphs: []

Then, like proper tags, add values to it. Example for global tag above:

    summary = some summary
    summary = and another summary

And for local (page) tags:

    -- paragraphs: some short text
    -- paragraphs: another short text

Main usage of composite tags is [TABLE statement](http://docs.adacore.com/aws-docs/templates_parser/template_statements.html#table-statement)
inside templates. All rules which applies to the normal template tags also
apply to composite tags

## <a name="using"></a>Using tags inside templates files

To use any tags inside template file you must surround selected tag with `{%`
and `%}`. For example, to get value of tag `Content` add to template file:

    {%Content%}

**Note:** All tags names are case-sensitive.

For complete documentation on how to use tags and what is available, please
refer to [Ada Web Server Templates documentation](http://docs.adacore.com/aws-docs/templates_parser/).
Especially:

- [Tags](http://docs.adacore.com/aws-docs/templates_parser/tags.html):
  part about Filters, Attributes and Predefined Tags.
- [Template statements](http://docs.adacore.com/aws-docs/templates_parser/template_statements.html):
  whole section.
- [Macros](http://docs.adacore.com/aws-docs/templates_parser/macros.html):
  whole section.

**Note:** In YASS you will use `{%` to start a tag (instead of `@_`) and `%}`
to end a tag (instead of `_@`).
