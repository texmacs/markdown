# TeXmacs markdown plugin

This plugin is a (for now one-way) converter to markdown format for 
[TeXmacs](http://www.texmacs.org/). It supports most of the standard Markdown 
syntax, plus much of TeXmacs' non-dynamic markup. In particular, labels and 
references, numbered environments and figures should work out of the box. The 
major exception are tables, but as a workaround the plugin offers the 
possibility of exporting them as html into the markdown file.

The plugin has been developed for its use in two specific websites, 
[Paperwhy](https://paperwhy.8027.org/) and appliedAI's 
[TransferLab](https://transferlab.appliedai.de/), and can use multiple 
extensions specific to the static website generator 
[Hugo](https://gohugo.io/). There might still be some code very specific to 
those sites, YMMV.

## Setup

Clone this repository into your `~/.TeXmacs/plugins` directory as `markdown`. 
For Linux / OSX this is:

```shell
git clone https://github.com/texmacs/markdown.git ~/.TeXmacs/plugins/markdown
```

For Windows, the path (usually?) is

```shell
\Users\YourUser\AppData\Roaming\TeXmacs\plugins
```

## Preferences

You can activate the plugin menu with `Tools -> Markdown plugin`. In it you 
will find a convenient `Export…` action, as well as all configurable 
preferences:

* `Flavour`: As of version 0.5.3 this is either `Hugo` or `vanilla`.
* `Paragraph width`: number of columns at which to wrap text in the generated 
  output. Set it to an empty value to deactivate wrapping.
* `Numbered sections?` Whether to prepend section numbers to numbered section 
  titles.
* `Export on save?` Whether to automatically export the document 
  **overwriting** the last exported markdown file, every time the TeXmacs 
  buffer is saved. The path to the exported file is stored in the document's 
  *auxiliary data* either as a relative or an absolute path with respect to 
  the document. Relative can be useful e.g. if the document is shared.

# Features

## Extensions to vanilla markdown

Besides standard Markdown, this plugin supports the following:

* Footnotes (and marginal notes, exported as footnotes for vanilla markdown).
* Links to equations and equation arrays. Labels are exported as HTML 
  `<span>` with identifiers sanitized and custom CSS. See 
  `extensions/tmmarkdown.css`.
* Most standard environments, both numbered and unnumbered. Localized and with 
  support for references and `<smart-ref>`.
* Numbered and unnumbered sections.
* Nested lists and styles applied to environments.

## Hugo support

In addition to the above, almost everything that Hugo supports can be 
converted from TeXmacs, including setting frontmatter values and extensions 
like footnotes and ~~striked through text~~.

Setting values for the frontmatter is suported via a dedicated macro defined 
in the style package `hugo.ts`. To use it go to `Document -> Style -> Add 
package -> Markdown -> hugo` or use the icon in the focus bar.

Now you can use the macro `<hugo-front>` to input any number of `key|value` 
pairs as arguments, one argument each. That is: insert the tag `<hugo-front>`, 
then use structured insert right to add two arguments and use the first for 
the key and the second for the value. Repeat as needed. It is possible to use 
strings, booleans (`true`, `false`) or dates (insert with `<date>`). 
Additionally, the macro `<pdf-name>` inserts the name of the file with `.pdf` 
appended. To enter a list, input `<tuple>` as the value and use structured 
insert right to add items. To input a dictionary, use `<dict>` and again use 
structured insert to create tuples of `key|value`.

### Supported shortcodes

All custom shortcodes are in `extensions/hugo/`.

* Figures are converted to `{{< figure … >}}`.
* For arbitrary shortcodes, use `<hugo-short>`, e.g. `<hugo-short|toc>` for 
  `{{< toc >}}`.
* Citations using `<cite>` and `<cite-detail>` (of any arity) are 
  automatically detected and converted to `{{< cite ref >}}`, and all 
  of them are gathered in the frontmatter as well, for indization by Hugo's 
  taxonomy system. The rendering of bibliography is done by `{{< 
  references >}}`.
* Marginal notes and figures are converted to `{{< sidenote … >}}` 
  and `{{< sidefigure … >}}`.

# Known issues

For an up-to date list, see the [issue tracker in 
GitHub](https://github.com/texmacs/markdown/issues/).

* The converter can break with malformed or unexpected input, like markup 
  inside tags whose values should be strings (although many cases are 
  “handled”).
* Error reporting is rather lacking. Run TeXmacs in a console to see stack 
  traces and such in case you are running into problems.
* EPS and PDF images are not supported by browsers. As a workaround allowing 
  to have both, use `<md-alt-image>`. The first argument is for TeXmacs and 
  the second one for the markdown export.
* The converter supports only one style. For instance all environments have 
  emphasized bodies and bold names, `<section>` is exported as `h1`, etc.

# License

[GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html).
