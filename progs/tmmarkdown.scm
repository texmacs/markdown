;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmarkdown.scm
;; DESCRIPTION : TeXmacs-stree to markdown-stree converter
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tmmarkdown)
  (:use (link ref-markup) (markdown-smart-ref) (markdown-utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counters
;; FIXME: this is rather hacky. Should rewrite everything using counter groups
;; Counters are tuples of (value "label") in order to be able to deactivate
;; them when interspersing unnumbered and numbered sections.
;; Counters have one parent. There are no counter groups.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define counters (make-ahash-table))
(define current-counter #f)  ; the counter which applies to a new label
(define labels #f)

(define (counter-parents which)
  (cdr (ahash-ref counters which '(()))))

(define (counter-children which)
  (list-filter
    (ahash-fold (lambda (key val acc) 
                  (cons (if (member which (cdr val)) key '()) acc))
                '()
                counters)
    nnull?))

(define (counter-value which)
  (caar (ahash-ref counters which (list (cons 0 "")))))

(define (counter-label which)
  (cdar (ahash-ref counters which (list (cons 0 "")))))

(define (counter-values which)
  "List of values of counter and parents (farthest ancestor last)"
  ; HACK: we remove zeros for missing (sub)sections
   (list-filter
    (map counter-value (cons which (counter-parents which)))
    (compose not zero?)))

(define (counter-labels which)
  "List of labels of counter and parents (farthest ancestor last), up to 
first empty label"
  (letrec ((up-to (lambda (what l)
                    (cond ((null? l) '())
                          ((== what (car l)) '())
                          (else (cons (car l) (up-to what (cdr l))))))))
    (up-to "" (map counter-label (cons which (counter-parents which))))))

(define (counter-set which value label)
  (let ((children (counter-children which))
        (parents (counter-parents which)))
    (map (cut counter-set <> 0 "") children)
    (ahash-set! counters which (cons (cons value label) parents))))

(define (counter-increase which)
  (with value (+ 1 (counter-value which))
    (counter-set which value (number->string value))))

(define (counter-new which . parents)
  (ahash-set! counters which (cons (cons 0 "") parents)))

(define (counter-deactivate which)
  (with value (counter-value which)
    (counter-set which value "")))

(define (counter->string which)
  (string-recompose (reverse (counter-labels which)) "."))

(define (make-counters)
  (counter-new 'default)
  (counter-new 'chapter)  ; TODO
  (counter-new 'h1)
  (counter-new 'h2 'h1)
  (counter-new 'h3 'h2 'h1)
  (counter-new 'env)
  (counter-new 'alg)
  (counter-new 'equation)
  (counter-new 'figure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for dispatching
;; TODO: use TeXmacs' logic-dispatch, export sessions, bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For some reason we always receive an stree, so we cannot use tm-file?
; because it expects its argument to be a tree and at some point queries a
; string for its tree-label and obviously fails... duh.
(define (is-file? x)
  (and (func? x 'document)
       (== 1 (length (select x '(body))))))

(define (keep x)
  "Recursively processes @x while leaving its func untouched."
  (cons (car x) (map texmacs->markdown* (cdr x))))

(define keep-verbatim identity)

(define (count action counter)
  "Returns @action (e.g. keep) after increasing @counter"
  (lambda ( . x)
    (counter-increase counter)
    (set! current-counter counter)
    (apply action x)))

(define (with-counter action counter)
  "Returns @action (e.g. keep) setting @counter to active, without incrementing"
  (lambda ( . x)
    (set! current-counter counter)
    (apply action x)))

(define (count-not action counter)
  "Returns @action after deactivating @counter"
  (lambda ( . x)
    (counter-deactivate counter)
    (apply action x)))

(define (change-to what)
  "Returns a fun (a . b) -> `(,what ,(texmacs->markdown* b)), or -> what if not a symbol"
  (lambda (x)
    (if (symbol? what)
        (cons what (map texmacs->markdown* (cdr x)))
        what)))

(define (skip-to . n)
  "Recursively processes @x dropping some of it first"
  (with num (if (null? n) 1 (car n))
    (lambda (x)
      (map texmacs->markdown* (list-drop x num)))))

(define (drop x)
  '())

(define (hrule-hack x)
  ; FIXME: this breaks inside quotations and whatnot. And it's ugly.
  '(document "" "---" ""))

(define (parse-link x)
  `(hlink ,(texmacs->markdown* (second x)) ,(third x)))

(define (parse-label x)
  (with label (sanitize-selector (second x))
    (ahash-set! labels label (counter->string current-counter))
    `(label ,label)))

(define (parse-reference x)
  (list (first x) (sanitize-selector (second x))))

(define (parse-smart-reference x)
  (texmacs->markdown* (ext-smart-ref x)))

(define (parse-make-eqref* x)
  (cond ((null? x) x)
        ((list-1? x) (parse-make-eqref* (first x)))
        ((list? x) (cons 'eqref (map parse-make-eqref* (cdr x))))
        (else x)))

(define (parse-make-eqref x)
  (texmacs->markdown* (parse-make-eqref* (cdr x))))

(define (make-env x type)
  "Numbered environments"
  `(,(string->symbol (string-append (symbol->string type) "-env"))
    ,(string-capitalize (symbol->string (first x)))
    ,(counter->string current-counter)
    ,(texmacs->markdown* (second x))))

(define (make-env* x type)
  "Unnumbered environments"
  `(,(string->symbol (string-append (symbol->string type) "-env*"))
    ,(string-drop-right (string-capitalize (symbol->string (first x))) 1)
    ,(texmacs->markdown* (second x))))

(define (parse-env x)
  (make-env x 'std))

(define (parse-env* x)
  (make-env* x 'std))

(define (parse-plain-env x)
  (make-env x 'plain))

(define (parse-plain-env* x)
  (make-env* x 'plain))

(define (parse-proof x)
  `(std-env* "Proof" ,(texmacs->markdown* (second x))))

(define (make-alg x extra)
  (when (nnull? extra)
    (set! extra `(concat " " ,extra)))
  `(document
    (strong (concat (localize "Algorithm") ,extra ": "))
    ,@(map texmacs->markdown* (cdr (second x)))))

(define (parse-alg x)
  (make-alg x (counter->string current-counter)))

(define (parse-alg* x)
  (make-alg x '()))

(define (parse-named-alg x)
  (make-alg `(dummy ,(third x)) (texmacs->markdown* (second x))))

(define (parse-image x)
  (if (func? x 'md-alt-image)
      (parse-image (third x))
      (with src (second x)
        (if (tm-is? src 'tuple)
            '(document "Cannot process embedded image")  ; TODO
            `(image ,src)))))

(define (is-figure? x)
  (and (member (car x)
               (numbered-unnumbered-append (figure-tag-list)))
       (not (string-contains? (symbol->string (car x)) "table"))))

(define (parse-figure-sub x)
  ; Example input:
  ; (big-figure (image "path-to.jpeg" "251px" "251px" "" "") 
  ;             (document "caption"))
  ; Or, when the "Figure num." in the figure is removed:
  ; (render-big-figure "" "Figure text" (image ...) (document "caption"))
  ;
  ; FIXME: We need to ignore the text until we write a Hugo shortcode
  ; implementing Figure text as TeXmacs.
  (let* ((offset (if (is-figure? x) 0 2))
         (img (tm-ref x offset))
         (caption `(concat (strong (concat (localize "Figure") " "
                                          ,(counter-label current-counter) ". "))
                           ,(texmacs->markdown* (tm-ref x (+ 1 offset)))))
         (src (if (tm-in? img '(image md-alt-image))
                  (tm-ref (parse-image img) 0)
                  '(document "Wrong image src"))))
    (list src caption)))

(define (parse-figure x)
  `(,(car x) ,@(parse-figure-sub x)))

(define (parse-marginal-figure x)
  (let* ((vpos (first (cdr x)))
         (args (parse-figure-sub `(small-figure ,@(cddr x)))))
    `(,(car x) ,vpos ,@args)))

(define (parse-with x)
  ; HACK: we end up calling ourselves with (with "stuff"), which
  ; actually is a malformed 'with tag but it's handy
  (cond ((== 1 (tm-length x)) (texmacs->markdown* (tm-ref x 0)))
        ((and (== "font-series" (tm-ref x 0))
              (== "bold" (tm-ref x 1)))
         `(strong ,(parse-with (cons 'with (cdddr x)))))
        ((and (== "font-shape" (tm-ref x 0))
              (== "italic" (tm-ref x 1)))
         `(em ,(parse-with (cons 'with (cdddr x)))))
        ((and (== "mode" (tm-ref x 0))
              (== "prog" (tm-ref x 1)))
         `(tt ,(parse-with (cons 'with (cdddr x)))))
        (else (parse-with (cons 'with (cdddr x))))))

(define (code-block syntax)
  (lambda (x)
    `(block ,syntax ,@(cdr x))))

(define (math->latex t)
 "Converts the TeXmacs tree @t into internal LaTeX representation"
 (with options '(("texmacs->latex:replace-style" . "on")
                 ("texmacs->latex:expand-macros" . "on")
                 ("texmacs->latex:expand-user-macros" . "off")
                 ("texmacs->latex:indirect-bib" . "off")
                 ("texmacs->latex:encoding" . "utf8")
                 ("texmacs->latex:use-macros" . "off"))
 (texmacs->latex t options)))

(define (md-fix-labels-in-row x)
  "Delete our generated labels from rows already having one, for append-latex-tag"
  (if (list>1? (select x '(:* label)))
      (replace-fun-list x '(((label "TMINCREMENT") . "")))
      x))

(define (md-add-backslashes-to-row t)
  "Append extra backslashes at the end of a !row in LaTeX tables"
  (if (and (func? t '!row) (list>1? t))
      (with cols (cdr t)
        `(!row ,@(cDr cols) (!concat ,(cAr cols) "\\\\\\\\")))
      t))

(define (md-fix-math-table t)
  (with rows (map md-fix-labels-in-row (cdr t))
    (if (not (list>1? rows)) t
      (let* ((last-row (cAr rows))
             (first-rows (cDr rows)))
        `(!table ,@(map md-add-backslashes-to-row first-rows) ,last-row)))))

(define (append-latex-tag x)
  (counter-increase current-counter)
  (let* ((label (sanitize-selector (cadr x)))
         (latex-tag (counter->string current-counter)))
    ; Special case: in eqnarrays we set special labels to trigger the generation
    ; of latex \tag{} in order to always see equation numbers.
    (if (== label "TMINCREMENT")
        (begin
          `(tag ,latex-tag))
        (begin
          (ahash-set! labels label latex-tag)
          ; leave the label to create anchors later
          `(!concat (label ,label) (tag ,latex-tag))))))

(define (md-math* t)
  (replace-fun-list t
   `((mathbbm . mathbb)
     ("-" . "\\-")
     ("*" . "\\*")
     (":" . "\\:")
     (({) . (lbrace))
     ((}) . (rbrace))
     ((left\{) . (left\lbrace))
     ((right\}) . (right\rbrace))
     (,(cut func? <> 'ensuremath) . ,cadr)
     (,(cut func? <> '!sub) .
       ,(lambda (x) (cons "\\_" (md-math* (cdr x)))))
     (,(cut func? <> 'label) . ,append-latex-tag )
     ; CAREFUL: this needs to happen before append-latex-tag, so it must go after
     (,(cut func? <> '!table) . ,md-fix-math-table))))

(define (parse-math x)
  ; HACK: use special labels to indicate numbered equations
  ; Needed because math->latex ignores and drops any (eq-number) in eqnarrays
  (when (and (func? x 'equation) (not (stree-contains? x '(label))))
    (set! x `(equation (document (concat ,@(cdadr x) (label "TMINCREMENT"))))))
  (with newx (replace-fun-list x '(((eq-number) . (label "TMINCREMENT"))))
    `(,(car x) ,(md-math* (math->latex newx)))))

(define (parse-menu n)
  "Documentation tags *menu"
  (lambda (t)
    `(tt (concat
           ,(list-intersperse
              (map texmacs->markdown* (list-drop (cdr t) n)) " -> ")))))

(define (make-header tag)
  (lambda (x)
    (if (preference-on? "texmacs->markdown:numbered-sections")
        (with label-name (counter->string current-counter)
                                        ; that space should be an nbsp
          `(,tag (concat ,label-name " " ,(map texmacs->markdown* (cdr x)))))
        `(,tag ,(map texmacs->markdown* (cdr x))))))

(define (parse-string s)
  (string-replace (string-replace s "_" "\\_") "*" "\\*"))

(define (parse-table x)
  ; TODO: handle different types tabular, tabular*, block*, etc.
  (cons 'table (tmtable-normalize (cons 'tformat (cdr x)))))

(define (parse-verbatim x)
  (cons 'tt (cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define conversion-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! conversion-hash <> <>) l)) 
     (list (list 'localize keep)
           (list 'strong keep)
           (list 'dfn (change-to 'strong))
           (list 'em keep)
           (list 'strike-through (change-to 'strike)) ; non-standard extension
           (list 'underline keep)  ; TODO: html extension
           (list 'hrule hrule-hack)
           (list 'name keep)  ; TODO: html extension
           (list 'tt keep)
           (list 'nbsp (change-to "&nbsp;"))
           (list 'samp (change-to 'tt))
           (list 'python parse-verbatim)
           (list 'cpp parse-verbatim)
           (list 'scm parse-verbatim)
           (list 'mmx parse-verbatim)
           (list 'scilab parse-verbatim)
           (list 'shell parse-verbatim)
           (list 'verbatim parse-verbatim)
           (list 'code* parse-verbatim)
           (list 'verbatim-code (code-block ""))
           (list 'code (code-block ""))
           (list 'scm-code (code-block "scheme"))
           (list 'cpp-code (code-block "c++"))
           (list 'mmx-code (code-block "mmx"))
           (list 'python-code (code-block "python"))
           (list 'scilab-code (code-block "scilab"))
           (list 'shell-code (code-block "shell"))
           (list 'doc-date keep)
           (list 'doc-author keep)
           (list 'author-data keep)
           (list 'author-name keep)
           (list 'author-email drop)
           (list 'abstract keep)
           (list 'document keep)
           (list 'quotation keep)
           (list 'acknowledgments (count parse-plain-env 'env))
           (list 'acknowledgments* parse-plain-env*)
           (list 'algorithm (count parse-alg 'alg))
           (list 'algorithm* parse-alg*)
           (list 'named-algorithm (count parse-named-alg 'alg))
           (list 'answer (count parse-plain-env 'env))
           (list 'answer* parse-plain-env*)
           (list 'axiom (count parse-env 'env))
           (list 'axiom* parse-env*)
           (list 'conjecture (count parse-env 'env))
           (list 'conjecture* parse-env*)
           (list 'convention (count parse-plain-env 'env))
           (list 'convention* parse-plain-env*)
           (list 'corollary (count parse-env 'env))
           (list 'corollary* parse-env*)
           (list 'definition (count parse-env 'env))
           (list 'definition* parse-env*)
           (list 'example (count parse-plain-env 'env))
           (list 'example* parse-plain-env*)
           (list 'exercise (count parse-plain-env 'env))
           (list 'exercise* parse-plain-env*)
           (list 'lemma (count parse-env 'env))
           (list 'lemma* parse-env*)
           (list 'notation (count parse-env 'env))
           (list 'notation* parse-env*)
           (list 'note (count parse-plain-env 'env))
           (list 'note* parse-plain-env*)
           (list 'problem (count parse-plain-env 'env))
           (list 'problem* parse-plain-env*)
           (list 'proof parse-proof)
           (list 'proposition (count parse-env 'env))
           (list 'proposition* parse-env*)
           (list 'question (count parse-plain-env 'env))
           (list 'question* parse-plain-env*)
           (list 'remark (count parse-plain-env 'env))
           (list 'remark* parse-plain-env*)
           (list 'solution (count parse-plain-env 'env))
           (list 'solution* parse-plain-env*)
           (list 'theorem (count parse-env 'env))
           (list 'theorem* parse-env*)
           (list 'warning (count parse-plain-env 'env))
           (list 'warning* parse-plain-env*)
           (list 'dueto keep)
           (list 'math parse-math)
           (list 'equation (with-counter parse-math 'equation))
           (list 'equation* parse-math)
           (list 'eqnarray (with-counter parse-math 'equation)) ; does this exist?
           (list 'eqnarray* (with-counter parse-math 'equation))
           (list 'concat keep)
           (list 'doc-title keep)
           (list 'doc-subtitle keep)
           (list 'doc-running-author keep)
           (list 'section (count (make-header 'h1) 'h1))
           (list 'section* (count-not (change-to 'h1) 'h1))
           (list 'subsection (count (make-header 'h2) 'h2))
           (list 'subsection* (count-not (change-to 'h2) 'h2))
           (list 'subsubsection (count (make-header 'h3) 'h3))
           (list 'subsubsection* (count-not (change-to 'h3) 'h3))
           (list 'paragraph (change-to 'para))
           (list 'subparagraph (change-to 'para))
           (list 'with parse-with)
           (list 'itemize keep)
           (list 'itemize-minus (change-to 'itemize))
           (list 'itemize-dot (change-to 'itemize))
           (list 'itemize-arrow (change-to 'itemize))
           (list 'enumerate keep)
           (list 'enumerate-numeric (change-to 'enumerate))
           (list 'enumerate-roman (change-to 'enumerate))
           (list 'enumerate-Roman (change-to 'enumerate))
           (list 'enumerate-alpha (change-to 'enumerate))
           (list 'enumerate-Alpha (change-to 'enumerate))
           (list 'item keep)
           (list 'cite keep-verbatim)
           (list 'cite-detail keep-verbatim)
           (list 'hlink parse-link)
           (list 'eqref parse-reference)
           (list 'label parse-label)
           (list 'flag drop)
           (list 'reference parse-reference)
           (list 'smart-ref parse-smart-reference)
           (list 'make-eqref parse-make-eqref)
           (list 'image parse-image)
           (list 'small-figure (count parse-figure 'figure))
           (list 'small-figure* parse-figure)
           (list 'render-small-figure parse-figure)
           (list 'big-figure (count parse-figure 'figure))
           (list 'big-figure* parse-figure)
           (list 'render-big-figure parse-figure)
           (list 'wide-figure (count parse-figure 'figure))
           (list 'wide-figure* parse-figure)
           (list 'marginal-figure (count parse-marginal-figure 'figure))
           (list 'marginal-figure* parse-marginal-figure)
           (list 'tabular parse-table)
           (list 'tabular* parse-table) ; centered contents
           (list 'block parse-table)
           (list 'block* parse-table)  ; centered contents
           (list 'wide-tabular parse-table)
           (list 'footnote keep)
           (list 'marginal-note keep)
           (list 'marginal-note* keep)
           (list 'todo keep)
           (list 'hide-preamble drop)
           (list 'TeXmacs (change-to "TeXmacs"))
           (list 'LaTeX (change-to "LaTeX"))
           (list 'LaTeX* (change-to "(La)TeX"))
           (list 'LaTeXe (change-to "LaTeXe"))
           (list 'BibTeX (change-to "BibTeX"))

           (list 'bibliography keep)  ; tfl extension only
           (list 'table-of-contents keep)  ; tfl extension only
           (list 'tags keep)  ; paperwhy extension (DEPRECATED)
           (list 'hugo-short keep)  ; Hugo extension (arbitrary shortcodes)
           (list 'hugo-front identity)  ; Hugo extension (frontmatter)
           (list 'text-dots (change-to "..."))
           
           (list 'md-alt-image (skip-to 2))
           ;; tm-doc style
           (list 'menu (parse-menu 0))
           (list 'submenu (parse-menu 1))
           (list 'subsubmenu (parse-menu 2))
           (list 'subsubsubmenu (parse-menu 3))
           (list 'subsubsubsubmenu (parse-menu 4))
           (list 'markup (change-to 'tt))
           (list 'explain-macro keep)
           (list 'src-var (skip-to))
           (list 'tmdoc-title (count (change-to 'h1) 'h1))
           (list 'tmdoc-copyright keep)
           (list 'tmdoc-license (change-to 'em))
           ))

;; Copy from smart ref table
(map (lambda (l)
       (ahash-set! conversion-hash (car l)
         (lambda (x) 
           (texmacs->markdown* 
            (ext-typed-ref* (first (cdr l)) (second (cdr l)) x)))))
     md-smart-ref-table)

(tm-define (texmacs->markdown* x)
  (cond ((string? x) (parse-string x))
        ((not (list>0? x)) x)
        ((symbol? (car x))
         (with fun (ahash-ref conversion-hash (car x))
           (if (!= fun #f)
               (fun x)
               ((skip-to 1) x))))
        (else (cons (texmacs->markdown* (car x))
                    (texmacs->markdown* (cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->markdown x)
  (with-global counters (make-ahash-table)
    (with-global current-counter 'default
      (with-global labels (make-ahash-table)
        (make-counters)
         `(markdown
           (labels ,(ahash-table->list labels))
           ,(if (is-file? x)
                (texmacs->markdown* (car (select x '(body document))))
                (texmacs->markdown* x)))))))
