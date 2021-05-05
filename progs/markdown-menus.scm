;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdown-menus.scm
;; DESCRIPTION : Simple (temporary?) menu-based config for the markdown plugin
;; COPYRIGHT   : (C) 2021 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (markdown-menus))

(define (markdown-test-flavour? flavour)
  (== (get-preference "texmacs->markdown:flavour") flavour))

(tm-define (markdown-set-flavour flavour)
  (:synopsis "Set the flavour of the texmacs->markdown converter")
  (:check-mark "v" markdown-test-flavour?)
  (set-preference "texmacs->markdown:flavour" flavour))

(menu-bind markdown-menu
  (-> "Flavour"
      ("Vanilla" (markdown-set-flavour "vanilla"))
      ("Hugo" (markdown-set-flavour "hugo")))
  ("Paragraph width" (noop)))

(menu-bind texmacs-extra-menu
  (if (supports-markdown?)
       (=> "Markdown" (link markdown-menu))))
