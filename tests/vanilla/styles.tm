<TeXmacs|2.1>

<style|generic>

<\body>
  Some regular text.

  <tt|Some tt text.>

  <verbatim|Some verbatim text>

  <strong|Some strong <em|and also emphasized text>>

  <strong|Some strong><em| and then some emphasized text><strong|, with
  punctuation at the end>

  <strong|Some strong<em| and also emphasized text >with blanks around the em
  tag>

  Thiswordhas<strong|strong>and<em|em>, but no spaces.\ 

  <\em>
    An emphasized paragraph with <strong|strong text in it>.
  </em>

  <verbatim|Some verbatim <strong|and bold> text>.

  <with|font-series|bold|Some bold text (using font-series bold)>

  <with|font-shape|italic|Some italics text (using font-series italic)>

  <\em>
    This is a test of emphasis

    with two paragraphs

    and a third one here
  </em>

  <\em>
    Another test of multi-paragraph style, with

    two paragraphs. <strong|And some strong text in the second one.>
  </em>

  <em|<\enumerate>
    <item>A list with emphasis around it

    <item><strong|The second item is strong too>

    <item>The third item has a sublist

    <strong|<\enumerate>
      <item>With item 3.1

      <item>And 3.2 both in bold
    </enumerate>>
  </enumerate>>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|save-aux|false>
  </collection>
</initial>