<TeXmacs|2.1>

<style|<tuple|generic|smart-ref|spanish>>

<\body>
  <\big-figure|<inactive|<image|||||>>>
    <label|fig:figure>Texto<text-dots>
  </big-figure>

  <\definition>
    <label|def:bayes-rule>Sean (<text-dots>)

    <\equation>
      h<rsup|\<ast\>><around*|(|x|)>\<assign\>I<rsub|<around*|(|<frac*|1|2>,1|]>><around*|(|\<bbb-E\><around*|(|Y\|X=x|)>|)>,<label|eq:bayes-rule>
    </equation>
  </definition>

  <\theorem>
    <label|thm:one>(Sea <math|H><text-dots>) La regla de Bayes
    <smart-ref|eq:bayes-rule> cumple:

    <\equation>
      R<around*|(|h<rsup|\<ast\>>|)>=min<rsub|h\<in\>H>
      R<around*|(|h|)>.<label|eq:risk>
    </equation>
  </theorem>

  Localization doesn't work so well for multiple refs together: A partir de
  la <smart-ref|def:bayes-rule|thm:one><text-dots>
</body>

<\initial>
  <\collection>
    <associate|info-flag|detailed>
    <associate|page-medium|paper>
    <associate|preamble|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|def:bayes-rule|<tuple|1|?>>
    <associate|eq:bayes-rule|<tuple|1|?>>
    <associate|eq:one|<tuple|2|1>>
    <associate|eq:risk|<tuple|2|?>>
    <associate|fig:figure|<tuple|1|1>>
    <associate|thm:one|<tuple|2|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|figure>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        asdfas
      </surround>|<pageref|auto-1>>
    </associate>
  </collection>
</auxiliary>