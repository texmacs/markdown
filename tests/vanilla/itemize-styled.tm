<TeXmacs|2.1>

<style|generic>

<\body>
  <\enumerate>
    <item>A normal list

    <item><em|With an emphasized second item>

    which has a second paragraph with a <strong|few strong words in it>.

    <em|<\enumerate>
      <item>And an alpha sublist

      <item>completely emphasized
    </enumerate>>
  </enumerate>

  <em|<\enumerate>
    <item>A list with emphasis around it

    <item><strong|The second item is strong too>

    <item>The third item has a sublist

    <strong|<\enumerate>
      <item>With item 3.1

      <item>And 3.2 both in bold
    </enumerate>>
  </enumerate>>

  <\enumerate>
    <item>An item

    <\itemize-dot>
      <item><strong|With a strong subitem>
    </itemize-dot>

    <item>Another item

    <em|With a second emphasized paragraph and a marginal note with em around
    it,<em|<marginal-note|normal|c|text>> then a sublist:>

    <strong|<\enumerate>
      <item>With item 1.1 being a very long item with lots of words so that
      there is a new line and we see what happens.

      <item>And 1.2 in bold, like the first one also with a text that will go
      beyond the maximum number of columns

      <\itemize-dot>
        <item>another sublist also has long items that will span more than
        whatever the number of columns blah blah

        <item>second item in the sublist
      </itemize-dot>
    </enumerate>>
  </enumerate>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>