Forward references: one [(1)](#eq-one), three [(3)](#eq-three), triangle 
[($\triangle$)](#eq-triangle).

A numbered equation:

<span id="eq-one" class="tm-eqlabel"></span>
\begin{equation}
  f (x) \equiv c. \label{eq-one}\tag{1}
\end{equation}

A reference to it: [(1)](#eq-one).

An unnumbered equation:

\\[ a = b. \\]

A numbered equation without label:

\begin{equation}
  c (X) =\mathbb{P} (Y \mid X) . \tag{2}
\end{equation}

Another numbered equation:

<span id="eq-three" class="tm-eqlabel"></span>
\begin{equation}
  f (x) \equiv c. \label{eq-three}\tag{3}
\end{equation}

This reference to it: [(3)](#eq-three) should render as: (3).

A custom label for an equation using `equation-lab`:

<span id="eq-triangle"></span>
\begin{equation}
  a = b \label{eq-triangle}\tag{$\triangle$}
\end{equation}

All references, again: one [(1)](#eq-one), three [(3)](#eq-three), triangle 
[($\triangle$)](#eq-triangle).
