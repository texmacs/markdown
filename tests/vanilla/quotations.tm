<TeXmacs|2.1>

<style|generic>

<\body>
  <\quotation>
    This text is quoted. It is a very long text spanning multiple lines, so
    we can see what happens with the caret at the beginning of the line.
  </quotation>

  <\quotation>
    This text is also quoted. It is a very long text spanning multiple lines,
    so we can see what happens with the caret at the beginning of the line.

    <\quotation>
      This is a quotation inside a quotation. It is a very long text spanning
      multiple lines, so we can see what happens with the caret at the
      beginning of the line.

      <\quotation>
        This is a third quotation inside the second quotation. It is a very
        long text spanning multiple lines, so we can see what happens with
        the caret at the beginning of the line.
      </quotation>
    </quotation>
  </quotation>

  <\itemize-dot>
    <item>This is a list item

    With a second paragraph

    <\quotation>
      And a quoted text. It is a very long text spanning multiple lines, so
      we can see what happens with the caret at the beginning of the line.
    </quotation>

    <item>This is the second item of the list

    <\itemize-dot>
      <item>It contains a sublist

      <item>And the second item of the sublist has

      <\quotation>
        some quoted text. It is a very long text spanning multiple lines, so
        we can see what happens with the caret at the beginning of the line.
      </quotation>
    </itemize-dot>
  </itemize-dot>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>