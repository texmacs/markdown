;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown format for TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown init-markdown))

(define-format markdown
  (:name "Markdown")
  (:suffix "md")
  ;(:recognize/:must-recognize markdown-recognizes?))
  )

;(lazy-define (convert markdown markdowntm) parse-markdown)
(lazy-define (convert markdown markdownout) serialize-markdown)
(lazy-define (convert markdown markdownout) serialize-markdown-document)
(lazy-define (convert markdown tmmarkdown) texmacs->markdown)
;(lazy-define (convert markdown markdowntm) markdown->texmacs)

;(converter markdown-document markdown-stree
;  (:function parse-markdown))

(converter markdown-stree markdown-document
  (:function serialize-markdown-document))

;(converter markdown-snippet markdown-stree
;  (:function parse-markdown))

(converter markdown-stree markdown-snippet
  (:function serialize-markdown))

;(converter markdown-stree texmacs-stree
;  (:function markdown->texmacs))

(converter texmacs-stree markdown-stree
  (:function texmacs->markdown))

