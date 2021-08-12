<TeXmacs|2.1>

<style|<tuple|generic|hugo>>

<\body>
  <\html-class|invertible>
    <\big-figure|<image|some_path.eps|3cm|||>>
      A numbered figure with just an image.
    </big-figure>
  </html-class>

  <\html-class|invertible>
    <\big-figure|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
      A numbered figure with just an image, using the md-alt-image tag
    </big-figure>
  </html-class>

  <\html-class|invertible>
    <\big-figure*|<image|some_path.eps|3cm|||>>
      An unnumbered figure with just an image.
    </big-figure*>
  </html-class>

  <\html-class|invertible>
    <\big-figure*|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
      An unnumbered figure with just an image, using the md-alt-image tag
    </big-figure*>
  </html-class>

  <\html-class|invertible>
    <\render-big-figure||Name with <math|a=b>
    math|<image|some_path.eps|3cm|||>>
      A named figure with just an image.
    </render-big-figure>
  </html-class>

  <\html-class|invertible>
    <\render-big-figure|>
      Name with <math|a=b> math
    <|render-big-figure|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
      A named figure with just an image, using the md-alt-image tag
    </render-big-figure>
  </html-class>

  <small-figure|<image|some_path.eps|2cm|||>|A small numbered figure>

  <small-table|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>|A small
  numbered table>

  <small-table*|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>|A small
  unnumbered table>

  <render-small-figure||A table name with <math|a=b>
  math|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>|A small named
  table>

  <\big-table|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>>
    A large numbered table
  </big-table>

  <\big-table*|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>>
    A large unnumbered table
  </big-table*>

  <\render-big-figure||A table name with <math|a=b>
  math|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>>
    A large named table
  </render-big-figure>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|1|?>>
    <associate|auto-11|<tuple|2|?>>
    <associate|auto-12|<tuple|2|?>>
    <associate|auto-13|<tuple|2|?>>
    <associate|auto-14|<tuple|1|?>>
    <associate|auto-15|<tuple|2|?>>
    <associate|auto-16|<tuple|3|?>>
    <associate|auto-17|<tuple|4|?>>
    <associate|auto-2|<tuple|2|1>>
    <associate|auto-3|<tuple|2|1>>
    <associate|auto-4|<tuple|2|?>>
    <associate|auto-5|<tuple|2|?>>
    <associate|auto-6|<tuple|2|?>>
    <associate|auto-7|<tuple|3|?>>
    <associate|auto-8|<tuple|1|?>>
    <associate|auto-9|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|>
      <\tuple|normal>
        A named figure with just an image.
      </tuple|<pageref|auto-5>>

      <\tuple|normal>
        A named figure with just an image, using the md-alt-image tag
      </tuple|<pageref|auto-6>>

      <tuple|normal|A small named table|<pageref|auto-10>>

      <\tuple|normal>
        A large named table
      </tuple|<pageref|auto-13>>
    </associate>
    <\associate|figure>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        A numbered figure with just an image.
      </surround>|<pageref|auto-1>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        A numbered figure with just an image, using the md-alt-image tag
      </surround>|<pageref|auto-2>>

      <\tuple|normal>
        An unnumbered figure with just an image.
      </tuple|<pageref|auto-3>>

      <\tuple|normal>
        An unnumbered figure with just an image, using the md-alt-image tag
      </tuple|<pageref|auto-4>>

      <tuple|normal|<surround|<hidden-binding|<tuple>|3>||A small numbered
      figure>|<pageref|auto-7>>
    </associate>
    <\associate|table>
      <tuple|normal|<surround|<hidden-binding|<tuple>|1>||A small numbered
      table>|<pageref|auto-8>>

      <tuple|normal|A small unnumbered table|<pageref|auto-9>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        A large numbered table
      </surround>|<pageref|auto-11>>

      <\tuple|normal>
        A large unnumbered table
      </tuple|<pageref|auto-12>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Stuff
      for tmmarkdown.scm> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-14><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Given
      the above, this is for markdownout.scm>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-15><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Other
      stuff> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-16><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Bibliography?>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-17><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>