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

(texmacs-module (markdown-menus)
  (:use (doc help-funcs) (tmmarkdown)))

(define (run-tests)
  "FIXME: make a module for test collection and execution"
  (with u (url-resolve "$TEXMACS_HOME_PATH/plugins/markdown/tests/run.scm" "r")
    (load (url->system u))))

(define (copy-mdtree)
  (let* ((sel (texmacs->markdown (tree->stree (selection-tree))))
         (fmt (format #f "~s" sel))
         (save (clipboard-get-import)))
    (clipboard-set-export "verbatim")
    (clipboard-set "primary" fmt)
    (clipboard-set-export save)))

(define (markdown-test-flavour? flavour)
  (== (get-preference "texmacs->markdown:flavour") flavour))

(tm-define (markdown-set-flavour flavour)
  (:synopsis "Set the flavour of the texmacs->markdown converter")
  (:check-mark "v" markdown-test-flavour?)
  (set-preference "texmacs->markdown:flavour" flavour))

(tm-define (markdown-set-paragraph-width w)
  (:synopsis "Set the number of columns for texmacs->markdown exports")
  (:argument w "Number of columns (empty for no limit)")
  (:default w (or (get-preference "texmacs->markdown:paragraph-width") ""))
  ; Anything which is not a number will be #f => no limit
  (set-preference "texmacs->markdown:paragraph-width" (string->number w)))

(menu-bind markdown-menu
  ("Export..." 
    (choose-file (buffer-exporter "markdown") "Export as Markdown" "markdown"))
  ---
  (group "Preferences")
  (-> "Flavour"
      ("Vanilla" (markdown-set-flavour "vanilla"))
      ("Hugo" (markdown-set-flavour "hugo")))
  ("Paragraph width" (interactive markdown-set-paragraph-width))
  ("Numbered sections" (toggle-preference "texmacs->markdown:numbered-sections"))
  ---
  ("Help" (load-help-article "markdown"))
  (when (with-developer-tool?)
      ---
      (group "Development")
      (when (selection-active?)
        ("Copy markdown tree" (copy-mdtree)))
      ("Run tests" (run-tests))))

(menu-bind texmacs-extra-menu
  (if (and (supports-markdown?) (markdown-menu-show?))
       (=> "Markdown" (link markdown-menu))))
