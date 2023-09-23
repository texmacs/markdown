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
  (:use (link ref-markup) (markdown-smart-ref) (markdown-utils)
        (convert tools tmtable)))

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
  (counter-new 'table)

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
  (cons (car x) (md-map texmacs->markdown* (cdr x))))

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
        (cons what (md-map texmacs->markdown* (cdr x)))
        what)))

(define (skip-to . n)
  "Recursively processes @x dropping some of it first"
  (with num (if (null? n) 1 (car n))
    (lambda (x)
      (md-map texmacs->markdown* (list-drop x num)))))

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
    ,@(md-map texmacs->markdown* (cdr (second x)))))

(define (parse-alg x)
  (make-alg x (counter->string current-counter)))

(define (parse-alg* x)
  (make-alg x '()))

(define (parse-named-alg x)
  (make-alg `(dummy ,(third x)) (texmacs->markdown* (second x))))

(define (parse-specified-alg x)
  (make-alg `(dummy (document ,@(append (cdr (second x)) (cdr (third x)))))
             (counter->string current-counter)))

(define (parse-specified-alg* x)
  (make-alg `(dummy (document ,@(append (cdr (second x)) (cdr (third x))))) '()))

(define (parse-image x)
  (if (func? x 'md-alt-image)
      (parse-image (third x))
      (with src (second x)
        (if (tm-is? src 'tuple)
            '(document "Cannot process embedded image")  ; TODO
            `(image ,src)))))

(define (figure-name s)
  "Name for numbered figures"
  `(strong (concat (localize ,s) " "
    ,(counter-label current-counter) ". ")))

(define (figure-name* x)
  "Name for unnumbered figures"
  `(strong (concat ,(texmacs->markdown* x) ". ")))

(define (figure-tag x)
  (with s (symbol->string (car x))
    (string->symbol (if (string-ends? s "*") (string-drop-right s 1) s))))

(define (render-figure-tag x)
  (string->symbol
   (substring (symbol->string (car x)) (string-length "render-"))))

(define (parse-figure-body x)
  (if (tm-in? x '(image md-alt-image))
      `(src . ,(tm-ref (parse-image x) 0))
      (with body (texmacs->markdown* x)
        (if (and (string? body) (string-null? body))
            `(body . ())
            `(body . ,body)))))

(define (parse-render-figure x)
  "For named figures."
  ; (render-big-figure "" "Figure text" (image ...) (document "caption"))
  (with args `((name . ,(figure-name* (tm-ref x 1)))
               ,(parse-figure-body (tm-ref x 2))
               (caption . ,(texmacs->markdown* (tm-ref x 3))))
      `(,(render-figure-tag x) ,@args)))

(define (parse-figure-sub name numbered?)
  ; (big-figure (image ...) (document "caption"))
  (lambda (x)
    (let* ((namer (if numbered? figure-name figure-name*))
           (args `((name . ,(namer name))
                 ,(parse-figure-body (tm-ref x 0))
                 (caption . ,(texmacs->markdown* (tm-ref x 1))))))
          `(,(figure-tag x) ,@args))))

(define parse-figure (parse-figure-sub "Figure" #t))
(define parse-figure* (parse-figure-sub "Figure" #f))
(define parse-table (parse-figure-sub "Table" #t))
(define parse-table* (parse-figure-sub "Table" #f))

(define (parse-marginal-figure-sub numbered?)
  "For marginal figures."
  ; (marginal-figure valign (image ...) (document "caption"))
  (lambda (x)
    (let* ((namer (if numbered? figure-name figure-name*))
           (args `((name . ,(namer "Figure"))
                   (valign . ,(md-marginal-style (tm-ref x 0)))
                   ,(parse-figure-body (tm-ref x 1))
                   (caption . ,(texmacs->markdown* (tm-ref x 2))))))
    `(,(figure-tag x) ,@args))))

(define parse-marginal-figure (parse-marginal-figure-sub #t))
(define parse-marginal-figure* (parse-marginal-figure-sub #f))

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
     ("*" . "\\*")
     (({) . (lbrace))
     ((}) . (rbrace))
     ((left\{) . (left\lbrace))
     ((right\}) . (right\rbrace))
     ; plugin extension: eqnarray-lab* and eqnarray-lab are used in equation
     ; arrays to add right-aligned labels. The prefix eq: is automatically added
     ; (see the style file markdown.ts)
     (,(cut func? <> 'eqnarraylabstar) .
       ,(lambda (x)
        `(tag ,(cadadr x))))
     (,(cut func? <> 'eqnarraylab) .
       ,(lambda (x)
         (let ((label (sanitize-selector (string-append "eq:" (cadadr x))))
               (tag (cadadr x)))
           (ahash-set! labels label tag)
           `(!concat (label ,label) (tag ,tag)))))
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
    `(tt (concat ,(list-intersperse 
                   (md-map texmacs->markdown* (list-drop (cdr t) n)) " -> ")))))

(define (make-header tag)
  (lambda (x)
    (if (preference-on? "texmacs->markdown:numbered-sections")
        (with label-name (counter->string current-counter)
                                      ; FIXME: that space should be an nbsp
          `(,tag (concat ,label-name " " ,(md-map texmacs->markdown* (cdr x)))))
        `(,tag ,(md-map texmacs->markdown* (cdr x))))))

(define (parse-string s)
  (string-replace (string-replace s "_" "\\_") "*" "\\*"))

(define (parse-tabular x)
  ; TODO: handle different types tabular, tabular*, block*, etc.
  (cons 'tabular (tmtable-normalize (cons 'tformat (cdr x)))))

(define (parse-verbatim x)
  (cons 'tt (cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define conversion-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! conversion-hash <> <>) l)) 
     (list 
      (list 'abstract keep)
      (list 'acknowledgments (count parse-plain-env 'env))
      (list 'acknowledgments* parse-plain-env*)
      (list 'algorithm (count parse-alg 'alg))
      (list 'algorithm* parse-alg*)
      (list 'answer (count parse-plain-env 'env))
      (list 'answer* parse-plain-env*)
      (list 'author-data keep)
      (list 'author-email drop)
      (list 'author-name keep)
      (list 'axiom (count parse-env 'env))
      (list 'axiom* parse-env*)
      (list 'bibliography keep)  ; tfl extension only
      (list 'BibTeX (change-to "BibTeX"))
      (list 'big-figure (count parse-figure 'figure))
      (list 'big-figure* parse-figure*)
      (list 'big-table (count parse-table 'table))
      (list 'big-table* parse-table*)
      (list 'block parse-tabular)
      (list 'block* parse-tabular)  ; TODO: centered contents
      (list 'cite-detail keep-verbatim)
      (list 'cite keep-verbatim)
      (list 'code (code-block ""))
      (list 'code* parse-verbatim)
      (list 'concat keep)
      (list 'conjecture (count parse-env 'env))
      (list 'conjecture* parse-env*)
      (list 'convention (count parse-plain-env 'env))
      (list 'convention* parse-plain-env*)
      (list 'corollary (count parse-env 'env))
      (list 'corollary* parse-env*)
      (list 'cpp-code (code-block "c++"))
      (list 'cpp parse-verbatim)
      (list 'definition (count parse-env 'env))
      (list 'definition* parse-env*)
      (list 'dfn (change-to 'strong))
      (list 'doc-author keep)
      (list 'doc-date keep)
      (list 'doc-running-author keep)
      (list 'doc-subtitle keep)
      (list 'doc-title keep)
      (list 'document keep)
      (list 'dueto keep)
      (list 'em keep)
      (list 'enumerate-alpha (change-to 'enumerate))
      (list 'enumerate-Alpha (change-to 'enumerate))
      (list 'enumerate keep)
      (list 'enumerate-numeric (change-to 'enumerate))
      (list 'enumerate-roman (change-to 'enumerate))
      (list 'enumerate-Roman (change-to 'enumerate))
      (list 'eqnarray* (with-counter parse-math 'equation))
      (list 'eqnarray (with-counter parse-math 'equation)) ; does this exist?
      (list 'eqref parse-reference)
      (list 'equation* parse-math)
      (list 'equation (with-counter parse-math 'equation))
      (list 'example (count parse-plain-env 'env))
      (list 'example* parse-plain-env*)
      (list 'exercise (count parse-plain-env 'env))
      (list 'exercise* parse-plain-env*)
      (list 'explain-macro keep)
      (list 'flag drop)
      (list 'footnote keep)
      (list 'hide-preamble drop)
      (list 'hlink parse-link)
      (list 'hrule hrule-hack)
      (list 'html-class keep)
      (list 'hugo-front identity)  ; Hugo extension (frontmatter)
      (list 'hugo-short keep)  ; Hugo extension (arbitrary shortcodes)
      (list 'image parse-image)
      (list 'itemize-arrow (change-to 'itemize))
      (list 'itemize-dot (change-to 'itemize))
      (list 'itemize keep)
      (list 'itemize-minus (change-to 'itemize))
      (list 'item keep)
      (list 'label parse-label)
      (list 'LaTeX (change-to "LaTeX"))
      (list 'LaTeX* (change-to "(La)TeX"))
      (list 'LaTeXe (change-to "LaTeXe"))
      (list 'lemma (count parse-env 'env))
      (list 'lemma* parse-env*)
      (list 'localize keep)
      (list 'make-eqref parse-make-eqref)
      (list 'marginal-figure (count parse-marginal-figure 'figure))
      (list 'marginal-figure* parse-marginal-figure*)
      (list 'marginal-note keep)
      (list 'marginal-note* keep)
      (list 'markup (change-to 'tt))
      (list 'math parse-math)
      (list 'md-alt-image (skip-to 2))
      (list 'menu (parse-menu 0))
      (list 'mmx-code (code-block "mmx"))
      (list 'mmx parse-verbatim)
      (list 'named-algorithm (count parse-named-alg 'alg))
      (list 'name keep)  ; TODO: html extension
      (list 'nbsp (change-to "&nbsp;"))
      (list 'no-break-here drop)
      (list 'no-break-here* drop)
      (list 'notation (count parse-env 'env))
      (list 'notation* parse-env*)
      (list 'note (count parse-plain-env 'env))
      (list 'note* parse-plain-env*)
      (list 'page-break drop)
      (list 'page-break* drop)
      (list 'paragraph (change-to 'para))
      (list 'paragraph* (change-to 'para))
      (list 'problem (count parse-plain-env 'env))
      (list 'problem* parse-plain-env*)
      (list 'proof parse-proof)
      (list 'proposition (count parse-env 'env))
      (list 'proposition* parse-env*)
      (list 'python-code (code-block "python"))
      (list 'python parse-verbatim)
      (list 'question (count parse-plain-env 'env))
      (list 'question* parse-plain-env*)
      (list 'quotation keep)
      (list 'reference parse-reference)
      (list 'remark (count parse-plain-env 'env))
      (list 'remark* parse-plain-env*)
      (list 'render-big-figure parse-render-figure)
      (list 'render-small-figure parse-render-figure)
      (list 'samp (change-to 'tt))
      (list 'scilab-code (code-block "scilab"))
      (list 'scilab parse-verbatim)
      (list 'scm-code (code-block "scheme"))
      (list 'scm parse-verbatim)
      (list 'section (count (make-header 'h1) 'h1))
      (list 'section* (count-not (change-to 'h1) 'h1))
      (list 'shell-code (code-block "shell"))
      (list 'shell parse-verbatim)
      (list 'small-figure (count parse-figure 'figure))
      (list 'small-figure* parse-figure*)
      (list 'small-table (count parse-table 'table))
      (list 'small-table* parse-table*)
      (list 'smart-ref parse-smart-reference)
      (list 'solution (count parse-plain-env 'env))
      (list 'solution* parse-plain-env*)
      (list 'specified-algorithm (count parse-specified-alg 'alg))
      (list 'specified-algorithm* parse-specified-alg*)
      (list 'src-var (skip-to))
      (list 'strike-through (change-to 'strike)) ; non-standard extension
      (list 'strong keep)
      (list 'submenu (parse-menu 1))
      (list 'subparagraph (change-to 'para))
      (list 'subparagraph* (change-to 'para))
      (list 'subsection (count (make-header 'h2) 'h2))
      (list 'subsection* (count-not (change-to 'h2) 'h2))
      (list 'subsubmenu (parse-menu 2))
      (list 'subsubsection (count (make-header 'h3) 'h3))
      (list 'subsubsection* (count-not (change-to 'h3) 'h3))
      (list 'subsubsubmenu (parse-menu 3))
      (list 'subsubsubsubmenu (parse-menu 4))
      (list 'table-of-contents keep)  ; tfl extension only
      (list 'tabular parse-tabular)
      (list 'tabular* parse-tabular) ; TODO: centered contents
      (list 'tags keep)  ; paperwhy extension (DEPRECATED)
      (list 'TeXmacs (change-to "TeXmacs"))
      (list 'text-dots (change-to "..."))
      (list 'theorem (count parse-env 'env))
      (list 'theorem* parse-env*)
      (list 'tmdoc-copyright keep)
      (list 'tmdoc-license (change-to 'em))
      (list 'tmdoc-title (count (change-to 'h1) 'h1))
      (list 'todo keep)
      (list 'tt keep)
      (list 'underline keep)  ; TODO: html extension
      (list 'verbatim-code (code-block ""))
      (list 'verbatim parse-verbatim)
      (list 'warning (count parse-plain-env 'env))
      (list 'warning* parse-plain-env*)
      (list 'wide-figure (count parse-figure 'figure))
      (list 'wide-figure* parse-figure*)
      (list 'wide-tabular parse-tabular)
      (list 'with parse-with)
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
