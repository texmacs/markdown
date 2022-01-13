<TeXmacs|2.1>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|markdown|0.2>

    <\src-purpose>
      Helper macros for the markdown plugin
    </src-purpose>

    <src-copyright|2022|Miguel de Benito Delgado>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  -- Alternate images for markdown and <TeXmacs> (first arg is used in
  markdown)

  <assign|md-alt-image|<macro|img|alt-img|<arg|img>>>

  <\active*>
    <\src-comment>
      Labels for equation arrays. Use on the rightmost column to add just
      text or text and labels to refer to.
    </src-comment>
  </active*>

  <assign|eqnarray-lab|<macro|lab|<htab|5mm><set-binding|<arg|lab>><around*|(|<arg|lab>|)><label|<merge|eq:|<arg|lab>>><flag|<merge|eq:|<arg|lab>>|blue>>>

  <assign|eqnarray-lab*|<macro|lab|<htab|5mm><around*|(|<arg|lab>|)>>>
</body>

<\initial>
  <\collection>
    <associate|global-author|Miguel de Benito Delgado>
    <associate|global-subject|>
    <associate|global-title|Macros for the TransferLab>
    <associate|page-medium|paper>
  </collection>
</initial>