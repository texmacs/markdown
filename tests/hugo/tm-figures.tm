<TeXmacs|2.1>

<style|<tuple|generic|hugo>>

<\body>
  <section|Numbered figures>

  <\big-figure|<image|some_path.eps|3cm|||>>
    A numbered figure with just an image.
  </big-figure>

  <\big-figure|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
    A numbered figure with just an image, using the md-alt-image tag.
  </big-figure>

  <\html-class|invertible>
    <\big-figure|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
      A numbered figure with just an image, using the md-alt-image tag. The
      figure is wrapped in html-class invertible.
    </big-figure>
  </html-class>

  <\html-class|invertible>
    <\big-figure|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>><md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
      A numbered figure with two images and md-alt-image tag, with html-class
      invertible.
    </big-figure>
  </html-class>

  <section|Unnumbered and named figures>

  <\big-figure*|<image|some_path.eps|3cm|||>>
    An unnumbered figure with just an image.
  </big-figure*>

  <\big-figure*|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
    An unnumbered figure with just an image, using the md-alt-image tag.
  </big-figure*>

  <\render-big-figure||Name with <math|a=b>
  math|<image|some_path.eps|3cm|||>>
    A named figure with just an image.
  </render-big-figure>

  <\render-big-figure|>
    Name with <math|a=b> math
  <|render-big-figure|<md-alt-image|<image|some_path.eps|3cm|||>|<image|some_path_webformat.svg|3cm|||>>>
    A named figure with just an image, using the md-alt-image tag.
  </render-big-figure>

  <section|Small figures>

  <small-figure|<image|some_path.eps|2cm|||>|A small numbered figure with an
  image>.

  <html-class|invertible|<small-figure|<image|some_path.eps|2cm|||>|A small
  numbered figure with an image. The figure is wrapped in html-class
  invertible.>>

  <small-figure*|<image|some_path.eps|2cm|||>|A small unnumbered figure with
  an image>.

  <html-class|invertible|<small-figure*|<image|some_path.eps|2cm|||>|A small
  unnumbered figure with an image. The figure is wrapped in html-class
  invertible.>>

  <section|Tables>

  <\big-table|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>>
    A large numbered table.
  </big-table>

  <\big-table*|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>>
    A large unnumbered table.
  </big-table*>

  <\render-big-figure||A table name with <math|a=b>
  math|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>>
    A large named table.
  </render-big-figure>

  <small-table|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>|A small
  numbered table>.

  <small-table*|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>|A small
  unnumbered table>.

  <render-small-figure||A table name with <math|a=b>
  math|<tabular|<tformat|<table|<row|<cell|a>|<cell|b>>>>>|A small named
  table>.
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-10|<tuple|2|?>>
    <associate|auto-11|<tuple|3|?>>
    <associate|auto-12|<tuple|5|?>>
    <associate|auto-13|<tuple|6|?>>
    <associate|auto-14|<tuple|6|?>>
    <associate|auto-15|<tuple|6|?>>
    <associate|auto-16|<tuple|4|?>>
    <associate|auto-17|<tuple|1|?>>
    <associate|auto-18|<tuple|1|?>>
    <associate|auto-19|<tuple|1|?>>
    <associate|auto-2|<tuple|1|?>>
    <associate|auto-20|<tuple|2|?>>
    <associate|auto-21|<tuple|2|?>>
    <associate|auto-22|<tuple|2|?>>
    <associate|auto-23|<tuple|2|?>>
    <associate|auto-24|<tuple|2|?>>
    <associate|auto-3|<tuple|2|?>>
    <associate|auto-4|<tuple|3|?>>
    <associate|auto-5|<tuple|4|?>>
    <associate|auto-6|<tuple|2|?>>
    <associate|auto-7|<tuple|2|?>>
    <associate|auto-8|<tuple|2|?>>
    <associate|auto-9|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|>
      <\tuple|normal>
        A named figure with just an image.
      </tuple|<pageref|auto-6>>

      <\tuple|normal>
        A named figure with just an image, using the md-alt-image tag
      </tuple|<pageref|auto-7>>

      <tuple|normal|A small named table|<pageref|auto-12>>

      <\tuple|normal>
        A large named table
      </tuple|<pageref|auto-15>>
    </associate>
    <\associate|figure>
      <tuple|normal|<\surround|<hidden-binding|<tuple>|1>|>
        A numbered figure with just an image.
      </surround>|<pageref|auto-1>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        A numbered figure with just an image, using the md-alt-image tag
      </surround>|<pageref|auto-2>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|3>|>
        A numbered figure with just an image. The figure is wrapped in
        html-class invertible.
      </surround>|<pageref|auto-3>>

      <\tuple|normal>
        An unnumbered figure with just an image.
      </tuple|<pageref|auto-4>>

      <\tuple|normal>
        An unnumbered figure with just an image, using the md-alt-image tag
      </tuple|<pageref|auto-5>>

      <tuple|normal|<surround|<hidden-binding|<tuple>|4>||A small numbered
      figure>|<pageref|auto-8>>

      <tuple|normal|<surround|<hidden-binding|<tuple>|5>||A small numbered
      figure with an image. The figure is wrapped in html-class
      invertible.>|<pageref|auto-9>>
    </associate>
    <\associate|table>
      <tuple|normal|<surround|<hidden-binding|<tuple>|1>||A small numbered
      table>|<pageref|auto-10>>

      <tuple|normal|A small unnumbered table|<pageref|auto-11>>

      <tuple|normal|<\surround|<hidden-binding|<tuple>|2>|>
        A large numbered table
      </surround>|<pageref|auto-13>>

      <\tuple|normal>
        A large unnumbered table
      </tuple|<pageref|auto-14>>
    </associate>
  </collection>
</auxiliary>