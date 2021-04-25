---

title: ""
authors: []
date: 2021-04-25
tags: []
summary: >
  
refs: 

---

# Section one<span name="ref:section-one"></span>

This section contains a numbered equation:

<span name="eqref:some-label"></span>
\begin{equation}
  f (x) \equiv c. \label{some-label}\tag{1}
\end{equation}

Which should be numbered (1). Reference: [(1)](#eqref:some-label).

# Section two<span name="ref:section-two"></span>

Another section. Reference to section three: undefined label: 'section-three'. Reference to section one: 1

**Theorem 1:**  <span name="ref:theorem-one"></span>This theorem should increase the label counter.

# Section three<span name="ref:section-three"></span>

Reference to theorem one: 1. Reference to section two: 2

