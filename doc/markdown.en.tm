<TeXmacs|2.1.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Markdown plug-in>

  <section|<TeXmacs> markdown plugin>

  This plugin is a (for now one-way) converter to markdown format for
  <hlink|<TeXmacs>|http://www.texmacs.org/>. It supports most of the standard
  Markdown syntax, plus much of <TeXmacs>' non-dynamic markup. In particular,
  labels and references, numbered environments and figures should work out of
  the box. The major exception are tables, but as a workaround the plugin
  offers the possibility of exporting them as html into the markdown file.

  The plugin has been developed for its use in two specific websites,
  <hlink|Paperwhy|https://paperwhy.8027.org/> and appliedAI's
  <hlink|TransferLab|https://transferlab.ai/>, and can use multiple
  extensions specific to the static website generator
  <hlink|Hugo|https://gohugo.io/>. There might still be some code very
  specific to those sites, YMMV.

  <subsection|Setup>

  Clone this repository into your <shell|~/.TeXmacs/plugins> directory as
  <shell|markdown> and checkout the <shell|master> branch. For Linux / OSX
  this is:

  <\shell-code>
    git clone -b master https://github.com/texmacs/markdown.git
    ~/.TeXmacs/plugins/markdown
  </shell-code>

  For Windows, the path (usually?) is\ 

  <\shell-code>
    \\Users\\YourUser\\AppData\\Roaming\\TeXmacs\\plugins
  </shell-code>

  <subsection|Preferences>

  You can activate the plugin menu with <menu|Tools|Markdown plugin>. In it
  you will find a convenient <submenu|Markdown|Export<text-dots>> action, as
  well as all configurable preferences:

  <\itemize>
    <item><submenu|Markdown|Flavour>: As of version 0.5.3 this is either
    <verbatim|Hugo> or <verbatim|vanilla>.\ 

    <item><submenu|Markdown|Paragraph width>: number of columns at which to
    wrap text in the generated output. Set it to an empty value to deactivate
    wrapping.

    <item><submenu|Markdown|Numbered sections?> Whether to prepend section
    numbers to numbered section titles.

    <item><submenu|Markdown|Export on save?> Whether to automatically export
    the document <strong|overwriting> the last exported markdown file, every
    time the <TeXmacs> buffer is saved. The path to the exported file is
    stored in the document's <em|auxiliary data> either as a relative or an
    absolute path with respect to the document. Relative can be useful e.g.
    if the document is shared.
  </itemize>

  <section|Features>

  <subsection|Additional <TeXmacs> macros>

  Use the style package <tt|markdown.ts> with <menu|Document|Style|Add
  package|Markdown|markdown> to enable:

  <\itemize>
    <item>Support for alternate image formats between <TeXmacs> and markdown
    via <explain-macro|md-alt-image|tm-image|md-image>. This is useful e.g.
    to provide SVG and EPS/PDF versions of images for print and web
    respectively. The same effect can be achieved with
    <explain-macro|specific|markdown|<text-dots>> and
    <explain-macro|specific|texmacs|<text-dots>>.

    <item>Macros for labels in equation arrays. Using positioning tricks with
    <explain-macro|htab> results in <LaTeX> code that <name|MathJax> does not
    support, namely <tt|\\hfill>. For this reason, we provide two macros
    <explain-macro|eqnarray-lab|lab> and <explain-macro|eqnarray-lab*|lab>
    which allow positioning of equation labels to the right in equation
    arrays. The first one also adds a <explain-macro|label|eq:lab> to be used
    in the document as <explain-macro|reference|eq:lab>, note the prefix
    <tt|eq:>.

    Symbols (e.g. <math|\<Delta\>> or <math|\<star\>>) are not supported for
    these labels as of v0.6.1 of the plugin.
  </itemize>

  <subsection|Extensions to vanilla markdown>

  Besides standard Markdown, this plugin supports the following:

  <\itemize-dot>
    <item>Footnotes (and marginal notes, exported as footnotes for vanilla
    markdown).

    <item>Links to equations and equation arrays. Labels are exported as HTML
    <verbatim|\<less\>span\<gtr\>> with identifiers sanitized and custom CSS.
    See <verbatim|extensions/tmmarkdown.css>.

    <item>Most standard environments, both numbered and unnumbered. Localized
    and with support for references and <explain-macro|smart-ref>.

    <item>Numbered and unnumbered sections.

    <item>Nested lists and styles applied to environments.
  </itemize-dot>

  <subsection|Hugo support>

  In addition to the above, almost everything that Hugo supports can be
  converted from <TeXmacs>, including setting frontmatter values and
  extensions like footnotes and <strike-through|striked through text>.

  Use the style package <tt|hugo.ts> with <menu|Document|Style|Add
  package|Markdown|hugo> to enable:

  <\itemize>
    <item>All of the markdown extensions above

    <item>Setting of values for the frontmatter. With
    <explain-macro|hugo-front> one can input any number of
    <verbatim|key\|value> pairs as arguments, one argument each. That is:
    insert the tag <explain-macro|hugo-front>, then use structured insert
    right to add two arguments and use the first for the key and the second
    for the value. Repeat as needed. It is possible to use strings, booleans
    (<verbatim|true>, <verbatim|false>) or dates (insert with
    <explain-macro|date>). Additionally, the macro <explain-macro|pdf-name>
    inserts the name of the file with <verbatim|.pdf> appended. To enter a
    list, input <explain-macro|tuple> as the value and use structured insert
    right to add items. To input a dictionary, use <explain-macro|dict> and
    again use structured insert to create tuples of <verbatim|key\|value>.
  </itemize>

  <subsubsection|Supported shortcodes>

  All custom shortcodes are in <verbatim|extensions/hugo/>.

  <\itemize-dot>
    <item>Figures are converted to <code*|{{\<less\> figure <text-dots>
    \<gtr\>}}>.

    <item>For arbitrary shortcodes, use <explain-macro|hugo-short>, e.g.
    <explain-macro|hugo-short|toc> for <code*|{{\<less\> toc \<gtr\>}}>.

    <item>Citations using <explain-macro|cite> and
    <explain-macro|cite-detail> (of any arity) are automatically detected and
    converted to <code*|{{\<less\> cite ref \<gtr\>}}>, and all of them are
    gathered in the frontmatter as well, for indization by Hugo's taxonomy
    system. The rendering of bibliography is done by <code*|{{\<less\>
    references \<gtr\>}}>.

    <item>Marginal notes and figures are converted to <code*|{{\<less\>
    sidenote <text-dots> \<gtr\>}}> and <code*|{{\<less\> sidefigure
    <text-dots> \<gtr\>}}>.
  </itemize-dot>

  <section|Known issues>

  For an up-to date list, see the <hlink|issue tracker in
  GitHub|https://github.com/texmacs/markdown/issues/>.

  <\itemize>
    <item>The converter can break with malformed or unexpected input, like
    markup inside tags whose values should be strings (although many cases
    are \Phandled\Q).

    <item>Error reporting is rather lacking. Run <TeXmacs> in a console to
    see stack traces and such in case you are running into problems.

    <item>EPS and PDF images are not supported by browsers. As a workaround
    allowing to have both, use <explain-macro|md-alt-image> in package
    <tt|markdown.ts> as explained above. The first argument is for <TeXmacs>
    and the second one for the markdown export.

    <item>The converter supports only one style. For instance all
    environments have emphasized bodies and bold names,
    <explain-macro|section> is exported as <verbatim|h1>, etc.
  </itemize>

  <section|License>

  <hlink|GPL v3|https://www.gnu.org/licenses/gpl-3.0.en.html>.
</body>

<\initial>
  <\collection>
    <associate|markdown-auto-export|../README.md>
  </collection>
</initial>