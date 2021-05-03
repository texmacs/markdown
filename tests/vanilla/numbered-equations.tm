<TeXmacs|1.99.18>

<style|generic>

<\body>
  A numbered equation:

  <\equation>
    f<around*|(|x|)>\<equiv\>c.<label|some-label>
  </equation>

  This reference will have a link to Equation <eqref|some-label>.

  An unnumbered equation:

  <\equation*>
    a=b.
  </equation*>

  Another numbered equation:

  <\equation>
    f<around*|(|x|)>\<equiv\>c.<label|another-label>
  </equation>

  This: <eqref|another-label> should be equal to this: (2).
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|preamble|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|another-label|<tuple|2|1>>
    <associate|some-label|<tuple|1|1>>
  </collection>
</references>