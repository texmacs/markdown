<TeXmacs|2.1>

<style|generic>

<\body>
  Simple math: <math|a=b>

  And an equation:

  <\equation*>
    f=\<cal-O\><around*|(|g|)>.
  </equation*>

  A very long math string with characters that should not appear first in a
  line: <math|a+b+c+d+e+f+g+h+i+j+k+l+m+n+p+q+r+s+t+u+v+w+x+y+z+1+2+3+4+5+6+7+8+9+0>.

  This is another concat: <math|a-b-c-d-e-f-g-h-i-j-k-l-m-n-p-q-r-s-t-u-v-w-x-y-z-1-2-3-4-5-6-7-8-9-0>.

  Another one: <math|a\<ast\>b\<ast\>c\<ast\>d\<ast\>e\<ast\>f\<ast\>g\<ast\>h\<ast\>i\<ast\>j\<ast\>k\<ast\>l\<ast\>m\<ast\>n\<ast\>p\<ast\>q\<ast\>r\<ast\>s\<ast\>t\<ast\>u\<ast\>v\<ast\>w\<ast\>x\<ast\>y\<ast\>z\<ast\>1\<ast\>2\<ast\>3\<ast\>4\<ast\>5\<ast\>6\<ast\>7\<ast\>8\<ast\>9\<ast\>0>.

  Final one: <math|a\<gtr\>b\<gtr\>c\<gtr\>d\<gtr\>e\<gtr\>f\<gtr\>g\<gtr\>h\<gtr\>i\<gtr\>j\<gtr\>k\<gtr\>l\<gtr\>m\<gtr\>n\<gtr\>p\<gtr\>q\<gtr\>r\<gtr\>s\<gtr\>t\<gtr\>u\<gtr\>v\<gtr\>w\<gtr\>x\<gtr\>y\<gtr\>z\<gtr\>1\<gtr\>2\<gtr\>3\<gtr\>4\<gtr\>5\<gtr\>6\<gtr\>7\<gtr\>8\<gtr\>9\<gtr\>0>.

  Now some 1. things 2. inside concats which should not start a line 3. 4. 5.
  6. 7. 8. 9. 0.

  This paragraph should split only at the quads:
  <math|+++++++++++++++++++<application-space|1em>++++++++++++++++++++++++<application-space|1em>++++++++++++++++++++++++<application-space|1em>+++++++++++++>

  And now an equation that should be adjusted to 80 cols

  <\eqnarray*>
    <tformat|<table|<row|<cell|F<around*|(|x|)>>|<cell|\<assign\>>|<cell|<big|int><rsub|-\<infty\>><rsup|x>f<around*|(|\<xi\>|)>*\<mathd\>x>>|<row|<cell|>|<cell|=>|<cell|<frac|1|1+<frac|1|1+<frac|1|1+<frac|1|n>>>>>>|<row|<cell|>|<cell|\<leqslant\>>|<cell|1+\<cdots\>+0+\<cdots\>+0+\<cdots\>+0+\<cdots\>+0+\<cdots\>+0+\<cdots\>+0+\<cdots\>+0>>>>
  </eqnarray*>
</body>

<\initial>
  <\collection>
    <associate|markdown-auto-export|simple-math.md>
    <associate|save-aux|false>
  </collection>
</initial>