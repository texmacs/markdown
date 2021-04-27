<TeXmacs|1.99.18>

<style|source>

<\body>
  -- FIXME: this should be a variadic macro for n key:val pairs

  -- FIXME: the scheme code is brittle: if val is not a string or a tuple of
  strings, it will break, possibly silently ignoring stuff.

  <assign|hugo-front|<macro|key|val|<flag|frontmatter|orange>>>
</body>

<\initial>
  <\collection>
    <associate|global-author|Miguel de Benito Delgado>
    <associate|global-subject|>
    <associate|global-title|Macros for the TransferLab>
    <associate|page-medium|paper>
  </collection>
</initial>