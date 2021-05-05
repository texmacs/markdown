# tm2md #

**tm2md** is a converter to markdown format
for [TeXmacs](http://www.texmacs.org/).

It supports all (most?) of the standard Markdown syntax, plus much of
TeXmacs' non-dynamic markup. In particular, labels and references,
numbered environments and figures should work out of the box. The
major exception are tables, but this can be overcome by exporting them
to html and pasting into the markdown file.

Furthermore, several extensions specific to the static website
generator [Hugo](https://gohugo.io) are supported but require writing
hugo templates or using specific TeXmacs style files. See below.


## Setup ##

* Clone this repository into your `~/.TeXmacs/progs/convert` directory
  as `markdown`:

```
mkdir -p ~/.TeXmacs/progs/convert
cd ~/.TeXmacs/progs/convert
git clone https://bitbucket.org/mdbenito/tm2md.git markdown
```

* Add the line `(use-modules (convert markdown init-markdown))` to
  your `my-init-texmacs.scm`.

## Known issues

This is **very brittle**. For instance, the converter breaks if:

* There is markup inside tags which expect strings (mostly fixed).
* There is malformed markup in some other way.
* strong or em tags are nested (e.g. em + em = regular in texmacs but 
  the converter keeps adding asterisks).

## To do ##

* Reverse markdown to TeXmacs conversion.
* Declare converter options in init file, and use
* Extract all Hugo extensions to a separate file, use overloading and
  extension of the dispatch hashmaps
* Use "converter environments"?
* Use TeXmacs' `logic-dispatch`?
* Extract embedded images.
* line-breaks and other markup in doc-data (e.g. in the doc-title)
  need to be properly handled if included in YAML metadata for Hugo.
* Support for tables.


## Hugo support ##

This plugin has been developed for its use in two specific websites.
See [here](https://bitbucket.org/mdbenito/paperwhy) for hugo
template examples and TeXmacs style files to make this work.

In addition to the standard markdown, almost everything that Hugo
supports can be converted from TeXmacs, including blackfriday
extensions like footnotes and ~~striked through text~~. 

Setting values for the frontmatter is suported via a dedicated tm tag
defined in `hugo.ts`. To use it type \hugo-front<enter> in texmacs
and input the key and value pair as arguments. Currently, only strings
and lists of strings are supported as values. To enter a list, input
\tuple<enter> and use texmacs' structured insert to add items.

Citations are automatically detected and converted to `{{< cite  ref>}}`,
and all of them are gathered in the frontmatter as well, for indization
by Hugo's taxonomy system.

For arbitrary shortcodes, use \hugo-short

## License ##

[GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html).
