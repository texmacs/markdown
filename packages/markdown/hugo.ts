<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|hugo|0.3>

    <\src-purpose>
      Helper macros for the Hugo extensions to the markdown plugin
    </src-purpose>

    <src-copyright|2021|Miguel de Benito Delgado>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|markdown>

  <\active*>
    <\src-comment>
      Frontmatter utilities
    </src-comment>
  </active*>

  <assign|hugo-short|<xmacro|args|<flag|<merge|hugo-|<arg|args|0>>|green>>>

  <assign|hugo-front|<xmacro|args|<flag|frontmatter parameters|orange>>>

  <assign|dict|<xmacro|kvs|dict (to do: display keys and values)>>

  <assign|pdf-name|<macro|<use-module|(markdown-utils)><extern|download-name>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|global-author|Miguel de Benito Delgado>
    <associate|global-subject|>
    <associate|global-title|Macros for the TransferLab>
    <associate|page-medium|paper>
  </collection>
</initial>