<TeXmacs|1.99.18>

<style|generic>

<\body>
  A label without context: <label|first-label>

  <section|Section one><label|section-one>

  This section contains a numbered equation:

  <\equation>
    f<around*|(|x|)>\<equiv\>c.<label|some-label>
  </equation>

  Which should be numbered (1). Reference: <eqref|some-label>.

  <section|Section two><label|section-two>

  Another section. Reference to section three: <reference|section-three>.
  Reference to section one: <reference|section-one>

  <\theorem>
    <label|theorem-one>This theorem should increase the label counter.
  </theorem>

  <section|Section three><label|section-three>

  Reference to theorem one: <reference|theorem-one>. Reference to section
  two: <reference|section-two>

  This is a broken reference: <reference|some invalid label>

  This is another broken reference, but to the label without context at the
  top: <reference|first-label>

  And this is a reference to an equation not using eqref:
  <reference|some-label>
</body>

<\initial>
  <\collection>
    <associate|global-title|references.tm>
    <associate|info-flag|detailed>
    <associate|page-medium|paper>
    <associate|preamble|false>
    <associate|save-aux|false>
  </collection>
</initial>