This is an equation array with three rows.

<span id="eq-one" class="tm-eqlabel"></span><span 
id="eq-three" class="tm-eqlabel"></span>
\begin{eqnarray*}
  \operatorname{cwECE}\_{k} & := & \mathbb{E}\_{C\_{k}} (| \mathbb{P} (Y = k
  \mid C\_{k}) \- C\_{k} |)  \label{eq-one}\tag{1}\\\\\\
  & = & \int^1\_{0} p (c\_{k}) | \mathbb{P} (Y = k \mid c\_{k}) \- c\_{k} |
  \mathrm{d} c\_{k}, \tag{2}\\\\\\
  \operatorname{cwEC} \mathrm{E} & := & \frac{1}{K} \sum\_{k}
  \operatorname{cwECE}\_{k} .  \label{eq-three}\tag{3}
\end{eqnarray*}

This is a reference to the first equation in the array: [(1)](#eq-one). And 
this is a reference to the last one: [(3)](#eq-three).

This is an equation array with three rows and custom labels using 
`eqnarray-lab` and `eqnarray-lab\*`, from the Markdown package

<span id="eq-problem-opt" 
class="tm-eqlabel"></span><span id="eq-UP" 
class="tm-eqlabel"></span>
\begin{eqnarray*}
  x^{\star} & = & \underset{x \in \bar{\Omega}}{\operatorname{argmin}} f (x)
  \label{eq-problem-opt}\tag{4}\\\\\\
  & \text{s.t.} & x \geqslant 0, \tag{LO}\\\\\\
  & \operatorname{and} & x \leqslant \delta . \label{eq-UP}\tag{UP}
\end{eqnarray*}

We can reference the second condition as [(UP)](#eq-UP). Note that symbols are 
not allowed as of version 0.6.1.
