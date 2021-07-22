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

(define (markdown-test-auto-export? value)
  (== (get-preference "texmacs->markdown:auto-export") value))

(tm-define (markdown-set-auto-export value)
  (:synopsis "Whether and where to automatically export documents")
  (:check-mark "v" markdown-test-auto-export?)
  (set-preference "texmacs->markdown:auto-export" value))

(define (markdown-test-table-format? value)
  (== (get-preference "texmacs->markdown:table-format") value))

(tm-define (markdown-set-table-format value)
  (:synopsis "How to export tables")
  (:check-mark "v" markdown-test-table-format?)
  (set-preference "texmacs->markdown:table-format" value))

(define (markdown-export)
  (lambda (u)
    (let* ((pref (get-preference "texmacs->markdown:auto-export"))
           (url (cond ((== pref "off") "")
                      ((== pref "relative") (url-delta (current-buffer) u))
                      ((== pref "absolute") u)
                      (else (string-append
                             "Bogus value of texmacs->markdown:auto-export: "
                             pref))))
           (dirty? (buffer-modified? (current-buffer))))
      (set-init-env "markdown-auto-export" (url->string url))
      ; set-init-env modifies the buffer, so we save it if it was clean
      (when (not dirty?)
        (buffer-save (current-buffer)))
      ((buffer-exporter "markdown") u))))

(menu-bind markdown-menu
  ("Export..." (choose-file (markdown-export) "Export as Markdown" "markdown"))
  ---
  (group "Preferences")
  ("Paragraph width" (interactive markdown-set-paragraph-width))
  ("Numbered sections?" (toggle-preference "texmacs->markdown:numbered-sections"))
  (-> "Flavour"
      ("Vanilla" (markdown-set-flavour "vanilla"))
      ("Hugo" (markdown-set-flavour "hugo")))
  (-> "Export tables as"
      (when #f
        ("Markdown" (markdown-set-table-format "none")))
      ("HTML" (markdown-set-table-format "html")))
  (-> "Export on save?"
      ("No" (markdown-set-auto-export "off"))
      ("With relative path" (markdown-set-auto-export "relative"))
      ("With absolute path" (markdown-set-auto-export "absolute")))
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
