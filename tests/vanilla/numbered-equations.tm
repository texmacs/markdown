<TeXmacs|2.1>

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

  A numbered equation without label:

  <\equation>
    c<around*|(|X|)>=\<bbb-P\><around*|(|Y\<mid\>X|)>.
  </equation>

  Another numbered equation:

  <\equation>
    f<around*|(|x|)>\<equiv\>c.<label|another-label>
  </equation>

  This: <eqref|another-label> should be equal to this: (3).

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|preamble|false>
    <associate|save-aux|false>
  </collection>
</initial>