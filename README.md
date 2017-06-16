# tm2md #

tm2md is a converter to markdown format for [TeXmacs](http://www.texmacs.org/).

## Setup ##

* Clone this repository into your `~/.TeXmacs/progs/convert` directory as `markdown`:

```
mkdir -p ~/.TeXmacs/progs/convert
cd ~/.TeXmacs/progs/convert
git clone https://bitbucket.org/mdbenito/tm2md.git markdown
```

* Add the line `(use-modules (convert markdown init-markdown))` to your 
`my-init-texmacs.scm`

## To do ##

* Use TeXmacs' `logic-dispatch`
* Clean up the mess with `with-global`
* Inverse markdown to TeXmacs conversion

## License ##

GPL v3