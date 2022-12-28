<TeXmacs|2.1>

<style|<tuple|generic|markdown>>

<\body>
  This is an equation array with three rows.\ 

  <\eqnarray*>
    <tformat|<table|<row|<cell|cwECE<rsub|k>>|<cell|\<assign\>>|<cell|\<bbb-E\><rsub|C<rsub|k>><around*|(|<around*|\||\<bbb-P\><around*|(|Y=k\<mid\>C<rsub|k>|)>
    - C<rsub|k>|\|>|)><eq-number><label|eq:one>>>|<row|<cell|>|<cell|=>|<cell|<big|int><rsup|1><rsub|0>p<around*|(|c<rsub|k>|)><around*|\||\<bbb-P\><around*|(|Y=k\<mid\>c<rsub|k>|)>-c<rsub|k>|\|>\<mathd\>c<rsub|k>
    ,<eq-number>>>|<row|<cell|cwEC\<Epsilon\>>|<cell|\<assign\>>|<cell|<frac|1|K><big|sum><rsub|k>cwECE<rsub|k>.<eq-number><label|eq:three>>>>>
  </eqnarray*>

  This is a reference to the first equation in the array: <eqref|eq:one>. And
  this is a reference to the last one: <eqref|eq:three>.

  This is an equation array with three rows and custom labels using
  <tt|eqnarray-lab> and <tt|eqnarray-lab*>, from the Markdown package

  <\eqnarray*>
    <tformat|<table|<row|<cell|x<rsup|\<star\>>>|<cell|=>|<cell|<below|argmin|x\<in\><wide|\<Omega\>|\<bar\>>>
    f<around*|(|x|)><eq-number><label|eq:problem-opt>>>|<row|<cell|>|<cell|<text|s.t.>>|<cell|x\<geqslant\>0,<eqnarray-lab*|LO>>>|<row|<cell|>|<cell|and>|<cell|x\<leqslant\>\<delta\>.<eqnarray-lab|UP>>>>>
  </eqnarray*>

  We can reference the second condition as <eqref|eq:UP>. Note that symbols
  are not allowed as of version 0.6.1.
</body>
