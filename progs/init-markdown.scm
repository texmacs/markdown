;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-markdown.scm
;; DESCRIPTION : Markdown format for TeXmacs
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure markdown)

(define (ignore var val) (noop))
(define-preferences
  ("texmacs->markdown:flavour" "vanilla" ignore)
  ("texmacs->markdown:paragraph-width" 79 ignore)
  ("texmacs->markdown:show-menu" "off" ignore))

(define-format markdown
  (:name "Markdown")
  (:suffix "md")
  ;(:recognize/:must-recognize markdown-recognizes?))
  )

;(lazy-define (markdown markdowntm) parse-markdown)
;(lazy-define (markdown markdowntm) markdown->texmacs)

(lazy-define (markdownout) serialize-markdown)
(lazy-define (markdownout) serialize-markdown-document)
(lazy-define (tmmarkdown) texmacs->markdown)

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


;; (import-from (markdown-menus))
(lazy-menu (markdown-menus) markdown-menu tools-menu)

(define (markdown-menu-show?)
  (== (get-preference "texmacs->markdown:show-menu") "on"))

(delayed (:idle 1000)
  (lazy-define-force tools-menu)
  (menu-bind tools-menu
    (former)
    ("Markdown plugin" (toggle-preference "texmacs->markdown:show-menu"))))
