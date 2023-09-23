# Contributing to the TeXmacs markdown plugin

Follow the setup instructions in [README.md](README.md) to get started.

**TODO:** this document is a stub.

## Code organization

Conversion is done in two stages:

1. From TeXmacs to an internal stree representation: `tmmarkdown.scm`. 
2. From the internal representation to markdown: `markdownout.scm`.

The plugin is loaded in `init-markdown.scm`, and some utilities are provided in
`markdown-utils.scm`. Extensions to TeXmacs menus are in `markdown-menus.scm`.

Tests are in `tests/` and are can be run from within TeXmacs with the markdown
menu.

## Conversion from TeXmacs to internal representation

The entry point is `tm->markdown*` in `tmmarkdown.scm`. The function takes a
TeXmacs tree and returns an slist of markdown elements. This is also a 
pre-processing step does things like dropping TeXmacs-specific info, adding
labels to equations, setting up counters, etc.

Parsing functions always return an slist. Some arguments are processed and
`tm->markdown*` is typically called on the remaining ones recursively.


## Conversion from internal representation to markdown

The entry point is `serialize-markdown` in `markdownout.scm`.

## Some suggestions for development

Like all TeXmacs code, the entry point for each module is located at the
bottom of the file.

Scheme sessions inside TeXmacs documents are very useful to prototype ideas.
First you want to add whatever tag it is that you are interested, say `mytag`
to the top of the document, then you read it into scheme with:

```scheme
(select (buffer-get-body (current-buffer)) '(:* mytag))
```

This will return a list of trees with the tag `mytag`. You can then pick any
one of them and process it with the `tm-` prefixed built-in functions:
* `(tm-ref x pos)`: returns the `pos`th child of `x`.
* `(tm->string x)`: returns a string representation of `x`.
* `(tm->markdown* x)`: returns an slist of markdown elements.

## Testing

Tests are located in `tests/` and are run from within TeXmacs with the markdown
menu. Each test reads a TeXmacs file and compares it with a homonymous markdown
file in the same directory.
