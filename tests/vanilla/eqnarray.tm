<TeXmacs|2.1>

<style|generic>

<\body>
  This is an equation array with three rows.\ 

  <\eqnarray*>
    <tformat|<table|<row|<cell|cwECE<rsub|k>>|<cell|\<assign\>>|<cell|\<bbb-E\><rsub|C<rsub|k>><around*|(|<around*|\||\<bbb-P\><around*|(|Y=k\<mid\>C<rsub|k>|)>
    - C<rsub|k>|\|>|)><eq-number><label|eq:one>>>|<row|<cell|>|<cell|=>|<cell|<big|int><rsup|1><rsub|0>p<around*|(|c<rsub|k>|)><around*|\||\<bbb-P\><around*|(|Y=k\<mid\>c<rsub|k>|)>-c<rsub|k>|\|>\<mathd\>c<rsub|k>
    ,<eq-number>>>|<row|<cell|cwEC\<Epsilon\>>|<cell|\<assign\>>|<cell|<frac|1|K><big|sum><rsub|k>cwECE<rsub|k>.<eq-number><label|eq:three>>>>>
  </eqnarray*>

  This is a reference to the first equation in the array: <eqref|eq:one>. And
  this is a reference to the last one: <eqref|eq:three>.
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
    <associate|save-aux|false>
  </collection>
</initial>