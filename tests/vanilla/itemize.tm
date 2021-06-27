<TeXmacs|2.1>

<style|generic>

<\body>
  Itemize dot:

  <\itemize-dot>
    <item>Item 1

    <item>Item 2 has multiple paragraphs.

    This is the second paragraph of item 2
  </itemize-dot>

  Itemize arrow:

  <\itemize-arrow>
    <item>Item 1

    <item>Item 2
  </itemize-arrow>

  Itemize minus

  <\itemize-minus>
    <item>Item 1

    <item>Item 2
  </itemize-minus>

  Enumerate

  <\enumerate>
    <item>Item 1 has a very long unique paragraph which should span more than
    the maximum of eighty columns set for the tests.

    <item>Item 2 has multiple paragraphs.

    This is the second paragraph of item 2. It contains an equation:

    <\equation*>
      a=b.
    </equation*>
  </enumerate>

  Enumerate numeric

  <\enumerate-numeric>
    <item>Item 1

    <item>Item 2
  </enumerate-numeric>

  Enumerate roman

  <\enumerate-roman>
    <item>Item 1

    <item>Item 2
  </enumerate-roman>

  Enumerate Roman

  <\enumerate-Roman>
    <item>Item 1

    <item>Item 2
  </enumerate-Roman>

  Enumerate alpha

  <\enumerate-alpha>
    <item>Item 1

    <item>Item 2
  </enumerate-alpha>

  Enumerate Alpha

  <\enumerate-Alpha>
    <item>Item 1

    <item>Item 2
  </enumerate-Alpha>

  Itemize with subitems:

  <\itemize-dot>
    <item>Item 1

    <\itemize-dot>
      <item>Subitem 1.1

      <item>Subitem 1.2
    </itemize-dot>

    <item>Item 2

    <item>Item 3

    <\itemize-dot>
      <item>Subitem 3.1

      With a second paragraph and then subitems

      <\itemize-dot>
        <item>Subitem 3.1.1

        With a second paragraph

        <item>Subitem 3.1.2
      </itemize-dot>
    </itemize-dot>
  </itemize-dot>

  Enumerate with subitems:

  <\enumerate>
    <item>Item 1

    <\enumerate>
      <item>Item 1.1

      <item>Item 1.2
    </enumerate>

    <item>Item 2
  </enumerate>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>