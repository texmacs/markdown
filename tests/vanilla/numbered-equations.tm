<TeXmacs|2.1>

<style|generic>

<\body>
  <\hide-preamble>
    \;
  </hide-preamble>

  Forward references: one <eqref|eq:one>, three <eqref|eq:three>, triangle
  <eqref|eq:triangle>.

  A numbered equation:

  <\equation>
    f<around*|(|x|)>\<equiv\>c.<label|eq:one>
  </equation>

  A reference to it: <eqref|eq:one>.

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
    f<around*|(|x|)>\<equiv\>c.<label|eq:three>
  </equation>

  This reference to it: <eqref|eq:three> should render as: (3).

  A custom label for an equation using <tt|equation-lab>:

  <equation-lab|<label|eq:triangle>a=b|\<triangle\>>

  All references, again: one <eqref|eq:one>, three <eqref|eq:three>, triangle
  <eqref|eq:triangle>.
</body>

<\initial>
  <\collection>
    <associate|info-flag|detailed>
    <associate|page-medium|paper>
    <associate|preamble|false>
    <associate|save-aux|false>
  </collection>
</initial>