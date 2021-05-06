<TeXmacs|1.99.18>

<style|manual>

<\body>
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

  Clone this repository into your <code*|~/.TeXmacs/plugins><nbsp>directory
  as <code*|markdown>. For Linux / OSX this is:

  <\shell-code>
    git clone https://bitbucket.org/mdbenito/tm2md.git
    ~/.TeXmacs/plugins/markdown
  </shell-code>

  For Windows, the path (usually?) is\ 

  <\shell-code>
    \\Users\\YourUser\\AppData\\Roaming\\TeXmacs\\plugins
  </shell-code>

  You can activate a menu with <submenu|Tools|Markdown plugin>.

  <subsection|Hugo support>

  In addition to standard markdown, almost everything that Hugo supports can
  be converted from <TeXmacs>, including setting frontmatter values and
  extensions like footnotes and <strike-through|striked through text>.

  Setting values for the frontmatter is suported via a dedicated macro
  defined in <code*|hugo.ts>. To use it first insert the
  <submenu|Markdown|Hugo> package in <subsubmenu|Document|Style|Add package>
  or using plus sign in the focus bar.

  Now you can type <verbatim|\\hugo-front> and input any number of key\|value
  pairs as arguments, one argument each. That is: type
  <verbatim|\\hugo-front>, then use structured insert right to add two
  arguments and use the first for the key and the second for the value.
  Repeat as needed. Currently, only strings, lists of strings and dates
  (insert with <verbatim|\\date>) are supported as values. To enter a list,
  input <verbatim|\\tuple> as the value and use structured insert right to
  add items.

  <subsubsection|Supported shortcodes>

  <\itemize-dot>
    <item>Figures are converted to <code*|{{\<less\> figure <text-dots>
    \<gtr\>}}>

    <item>For arbitrary shortcodes, use <verbatim|\\hugo-short>.

    <item>Citations are automatically detected and converted to
    <code*|{{\<less\> cite ref \<gtr\>}}>, and all of them are gathered in
    the frontmatter as well, for indization by Hugo's taxonomy system.

    <item>Probably more<text-dots>
  </itemize-dot>

  <section|Known issues>

  <\itemize>
    <item>The converter can break with malformed or unexpected input, like
    markup inside tags whose values should be strings (although many cases
    are \Phandled\Q)

    <item>Error reporting is rather lacking. Run <TeXmacs> in a console to
    see stack traces and such in case you are running into problems.

    <item>No tables (yet)!

    <item>Images must be linked, not embedded in the document.
  </itemize>

  <section|To do>

  <\itemize>
    <item>Reverse markdown to <TeXmacs> conversion.

    <item>Declare converter options in init file, and use.

    <item>Extract all Hugo extensions to a separate file, use overloading and
    extension of the dispatch hashmaps.

    <item>Use \Pconverter environments\Q?

    <item>Use <TeXmacs>' <code*|logic-dispatch>?

    <item>line-breaks and other markup in doc-data (e.g. in the doc-title)
    need to be properly handled if included in YAML metadata for Hugo.

    <item>Support for tables.

    <item>Extract embedded images.
  </itemize>

  <section|License>

  <hlink|GPL v3|https://www.gnu.org/licenses/gpl-3.0.en.html>.
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.2|?>>
    <associate|auto-4|<tuple|1.2.1|?>>
    <associate|auto-5|<tuple|2|?>>
    <associate|auto-6|<tuple|3|?>>
    <associate|auto-7|<tuple|4|?>>
    <associate|footnote-1|<tuple|1|?>>
    <associate|footnr-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      T<rsub|<space|-0.4spc><move|<resize|<with|math-level|<quote|0>|E>||||0.5fn>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn>|0fn|-0.1fn>>
      markdown plugin <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1>

      <with|par-left|<quote|1tab>|Setup <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <with|par-left|<quote|1tab>|Hugo support
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <with|par-left|<quote|1tab>|Known issues
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|1tab>|To do <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5>>

      <with|par-left|<quote|1tab>|License
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>>
    </associate>
  </collection>
</auxiliary>