<TeXmacs|2.1.2>

<style|generic>

<\body>
  This should not export to markdown:

  <\itemize-dot>
    <item><TeXmacs>-only: <specific|texmacs|texmacs content>

    <item><LaTeX> only: <specific|latex|something here>

    <item>Screen only: <specific|screen|Some info>

    <item>HTML only: <specific|html|Some tag>

    <item>Printer only: <specific|printer|Only for paper>
  </itemize-dot>

  And here is some markdown-specific content: <specific|markdown|Hello
  <strong|hugo>>
</body>

<\initial>
  <\collection>
    <associate|markdown-auto-export|test.md>
    <associate|prog-scripts|python>
  </collection>
</initial>