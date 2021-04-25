<TeXmacs|1.99.18>

<style|generic>

<\body>
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

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|another-label|<tuple|2|1|numbered-equations.tm>>
    <associate|auto-1|<tuple|1|?|numbered-equations.tm>>
    <associate|auto-2|<tuple|2|?|numbered-equations.tm>>
    <associate|auto-3|<tuple|3|?|numbered-equations.tm>>
    <associate|section-one|<tuple|1|?|numbered-equations.tm>>
    <associate|section-three|<tuple|3|?|numbered-equations.tm>>
    <associate|section-two|<tuple|2|?|numbered-equations.tm>>
    <associate|some-label|<tuple|1|1|numbered-equations.tm>>
    <associate|theorem-one|<tuple|1|?|numbered-equations.tm>>
  </collection>
</references>