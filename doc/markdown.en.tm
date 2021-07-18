<TeXmacs|2.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Markdown plug-in>

  <section|<TeXmacs> markdown plugin>

  This plugin is a (for now one-way) converter to markdown format for
  <hlink|<TeXmacs>|http://www.texmacs.org/>. It supports most of the standard
  Markdown syntax, plus much of <TeXmacs>' non-dynamic markup. In particular,
  labels and references, numbered environments and figures should work out of
  the box. The major exception is tables, but this can be overcome by
  exporting them to html and pasting into the markdown file.

  The plugin has been developed for its use in two specific websites,
  <hlink|Paperwhy|https://paperwhy.8027.org/> and appliedAI's
  <hlink|TransferLab|https://transferlab.appliedai.de/>, and can use multiple
  extensions specific to the static website generator
  <hlink|Hugo|https://gohugo.io/>. There might still be some code very
  specific to those sites, YMMV.

  <subsection|Setup>

  Clone this repository into your <shell|~/.TeXmacs/plugins> directory as
  <shell|markdown>. For Linux / OSX this is:

  <\shell-code>
    git clone https://github.com/texmacs/markdown.git
    ~/.TeXmacs/plugins/markdown
  </shell-code>

  For Windows, the path (usually?) is\ 

  <\shell-code>
    \\Users\\YourUser\\AppData\\Roaming\\TeXmacs\\plugins
  </shell-code>

  You can activate a menu with <menu|Tools|Markdown plugin>.

  <subsection|Extensions to vanilla markdown>

  Besides standard Markdown, this plugins does the following:

  <\itemize-dot>
    <item>Footnotes (and marginal notes, exported as footnotes for vanilla
    markdown).

    <item>Links to equations and equation arrays. Labels are exported as HTML
    <verbatim|\<less\>span\<gtr\>> with identifiers sanitized and custom CSS.
    See <verbatim|extensions/tmmarkdown.css>.

    <item>Support for most standard environments, both numbered and
    unnumbered. Localized and with support for references and
    <explain-macro|smart-ref>.

    <item>Probably more<text-dots>
  </itemize-dot>

  <subsection|Hugo support>

  In addition to standard markdown, almost everything that Hugo supports can
  be converted from <TeXmacs>, including setting frontmatter values and
  extensions like footnotes and <strike-through|striked through text>.

  Setting values for the frontmatter is suported via a dedicated macro
  defined in <code*|hugo.ts>. To use it first insert the <menu|Markdown|Hugo>
  package in <menu|Document|Style|Add package> or using the plus sign in the
  focus bar.

  Now you can use <explain-macro|hugo-front> to input any number of
  <verbatim|key\|value> pairs as arguments, one argument each. That is:
  insert the tag <explain-macro|hugo-front>, then use structured insert right
  to add two arguments and use the first for the key and the second for the
  value. Repeat as needed. It is possible to use strings, booleans (\Ptrue\Q,
  \Pfalse\Q) or dates (insert with <explain-macro|date>). Additionally, the
  macro <explain-macro|pdf-name> inserts the name of the file with
  <verbatim|.pdf> appended. To enter a list, input <explain-macro|tuple> as
  the value and use structured insert right to add items. To input a
  dictionary, use <explain-macro|dict> and again use structured insert to
  create tuples of <verbatim|key\|value>.

  <subsubsection|Supported shortcodes>

  All custom shortcodes are in <verbatim|extensions/hugo/>.

  <\itemize-dot>
    <item>Figures are converted to <code*|{{\<less\> figure <text-dots>
    \<gtr\>}}>.

    <item>For arbitrary shortcodes, use <explain-macro|hugo-short>, e.g.
    <explain-macro|hugo-short|toc> for <code*|{{\<less\> toc \<gtr\>}}>.

    <item>Citations are automatically detected and converted to
    <code*|{{\<less\> cite ref \<gtr\>}}>, and all of them are gathered in
    the frontmatter as well, for indization by Hugo's taxonomy system. The
    rendering of bibliography is done by <code*|{{\<less\> references
    \<gtr\>}}>.

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
    are \Phandled\Q)

    <item>Error reporting is rather lacking. Run <TeXmacs> in a console to
    see stack traces and such in case you are running into problems.

    <item>EPS and PDF images are not supported by browsers. As a workaround
    allowing to have both, use <explain-macro|md-alt-image>. The first
    argument is for <TeXmacs> and the second one for the markdown export.

    <item>The converter supports only one style. For instance all
    environments have emphasized bodies and bold names,
    <explain-macro|section> is exported as <verbatim|h1>, etc.
  </itemize>

  <section|License>

  <hlink|GPL v3|https://www.gnu.org/licenses/gpl-3.0.en.html>.
</body>

<initial|<\collection>
</collection>>