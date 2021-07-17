A label without context: <span id="first-label"></span>

# 1 Section one
<span id="section-one"></span>

This section contains a numbered equation:

<span id="some-label"></span>
\begin{equation}
  f (x) \equiv c. \label{some-label}\tag{1}
\end{equation}

Which should be numbered (1). Reference: [(1)](#some-label).

# 2 Section two
<span id="section-two"></span>

Another section. Reference to section three: [3](#section-three). Reference to 
section one: [1](#section-one)

**Theorem 1:** *<span id="theorem-one"></span>This theorem should increase the 
label counter.*

# 3 Section three
<span id="section-three"></span>

Reference to theorem one: [1](#theorem-one). Reference to section two: 
[2](#section-two)

This is a broken reference: [undefined label: 
'some-invalid-label'](#some-invalid-label)

This is another broken reference, but to the label without context at the top: 
[](#first-label)

And this is a reference to an equation not using eqref: [1](#some-label)