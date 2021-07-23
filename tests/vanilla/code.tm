<TeXmacs|2.1>

<style|generic>

<\body>
  Generic code: <code*|same line>

  Generic fence:

  <\code>
    Generic code here
  </code>

  Python code: <python|print("Hi Joris")>

  Python fence:

  <\python-code>
    from module import magic

    magic.do()
  </python-code>

  Shell: <shell|some shell code>

  Shell fence:

  <\shell-code>
    cd /pub

    more beer
  </shell-code>

  Cpp: <cpp|string magic_fun(string s);>

  Cpp fence:

  <\cpp-code>
    string magic_fun(string s) {

    \ \ \ \ \ return s;

    \ \ \ \ \ }
  </cpp-code>

  Scheme: <scm|(use-modules ((ice-42 magic) :select (schtuff)))>

  <\scm-code>
    (use-modules ((ice-42 magic)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ :select (schtuff)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ :renamer (<code|symbol-prefix-proc> 'foo:)))
  </scm-code>

  \;
</body>

<\initial>
  <\collection>
    <associate|save-aux|false>
  </collection>
</initial>