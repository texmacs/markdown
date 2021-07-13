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
  (:use (link ref-markup) (smart-ref-table) (utils)))

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
  (counter-new 'environment)
  (counter-new 'equation)
  (counter-new 'figure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for the transformation of strees and dispatcher
;; TODO: use TeXmacs' logic-dispatch, export sessions, bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-fun-sub where what? by)
  (if (npair? where) (if (what? where) (by where) where)
      (cons (if (what? (car where)) (by (car where))
                (replace-fun-sub (car where) what? by))
            (replace-fun-sub (cdr where) what? by))))

; This looks familiar... :/
(define (replace-fun where what by)
 (cond ((not (procedure? what))
        (replace-fun where (cut == <> what) by))
       ((not (procedure? by))
        (replace-fun where what (lambda (x) by)))
       (else (replace-fun-sub where what by))))

(define (replace-fun-list where rules)
  (if (and (list>0? rules) (pair? (car rules)))
      (replace-fun (replace-fun-list where (cdr rules))
                   (caar rules) (cdar rules))
      where))

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

(define (skip x)
  "Recursively processes @x and drops its func."
  ;(display* "Skipping into " (car x) "\n")
  (map texmacs->markdown* (cdr x)))

(define (drop x)
  ;(display* "Dropped " (car x) " !\n")
  '())

(define (hrule-hack x)
  ; FIXME: this breaks inside quotations and whatnot. And it's ugly.
  '(document "" "---" ""))

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

(define (parse-env x)
  (list (first x) (counter->string current-counter) 
        (texmacs->markdown* (second x))))

(define (parse-image x)
  (with src (tm-ref x 0)
    (if (tm-is? src 'tuple)
        '(document "Cannot process embedded image")  ; TODO
        `(image ,src))))

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
         (caption `(concat (strong  (localize "Figure") " "
                                    ,(counter-label current-counter)
                                    ". ")
                           ,(texmacs->markdown* (tm-ref x (+ 1 offset)))))
         (src (if (tm-is? img 'image) 
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

(define (md-fix-math-row t)
  "Append backslashes to last item in a !row"
  (if (and (func? t '!row) (list>1? t))
      (with cols (cdr t)
        `(!row ,@(cDr cols) (!concat ,(cAr cols) "\\\\\\\\")))
      t))

(define (md-fix-math-table t)
  "Append extra backslashes at the end of !rows in LaTeX tables"
  (if (not (list>1? (cdr t))) t  ; Nothing to do with only one row
      (let* ((rows (cdr t))
             (last-row (cAr rows))
             (first-rows (cDr rows)))
        `(!table ,@(map md-fix-math-row first-rows) ,last-row))))

(define (md-math* t)
  (replace-fun-list t
   `((mathbbm . mathbb)
     ((_) . "\\_")
     (({) . (lbrace))
     ((}) . (rbrace))
     ((left\{) . (left\lbrace))
     ((right\}) . (right\rbrace))
     (,(cut func? <> '!table) . ,md-fix-math-table)
     (,(cut func? <> 'ensuremath) . ,cadr)
     (,(cut func? <> '!sub) . 
       ,(lambda (x) (cons "\\_" (cdr x))))
     (,(cut func? <> 'label) .   ; append latex tags to labels
       ,(lambda (x)
          (with label-name (counter->string current-counter)
            (with label (sanitize-selector (cadr x))
              (ahash-set! labels label label-name)
              ; leave the label to create anchors later
              (list '!concat `(label ,label) `(tag ,label-name)))))))))

(define (parse-math x)
  `(,(car x) ,(md-math* (math->latex x))))

(define (parse-menu n)
  "Documentation tags *menu"
  (lambda (t)
    `(tt ,(string-concatenate (list-intersperse (list-drop (cdr t) n) " -> ")))))

(define (make-header tag)
  (lambda (x)
    (if (preference-on? "texmacs->markdown:numbered-sections")
        (with label-name (counter->string current-counter)
                                        ; that space should be an nbsp
          `(,tag (concat ,label-name " " ,(map texmacs->markdown* (cdr x)))))
        `(,tag ,(map texmacs->markdown* (cdr x))))))

(define (parse-string s)
  (string-replace (string-replace s "_" "\\_") "*" "\\*"))

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
           (list 'hrule hrule-hack)
           (list 'tt keep)
           (list 'nbsp (change-to "&nbsp;"))
           (list 'samp (change-to 'tt))
           (list 'python (change-to 'tt))
           (list 'cpp (change-to 'tt))
           (list 'scm (change-to 'tt))
           (list 'mmx (change-to 'tt))
           (list 'scilab (change-to 'tt))
           (list 'shell (change-to 'tt))
           (list 'verbatim (change-to 'tt))
           (list 'code* (change-to 'tt))
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
           (list 'acknowledgments (count parse-env 'env))
           (list 'acknowledgments* keep)
           (list 'algorithm (count parse-env 'env))
           (list 'algorithm* keep)
           (list 'answer (count parse-env 'env))
           (list 'answer* keep)
           (list 'axiom (count parse-env 'env))
           (list 'axiom* keep)
           (list 'conjecture (count parse-env 'env))
           (list 'conjecture* keep)
           (list 'convention (count parse-env 'env))
           (list 'convention* keep)
           (list 'corollary (count parse-env 'env))
           (list 'corollary* keep)
           (list 'definition (count parse-env 'env))
           (list 'definition* keep)
           (list 'example (count parse-env 'env))
           (list 'example* keep)
           (list 'exercise (count parse-env 'env))
           (list 'exercise* keep)
           (list 'lemma (count parse-env 'env))
           (list 'lemma* keep)
           (list 'notation (count parse-env 'env))
           (list 'notation* keep)
           (list 'problem (count parse-env 'env))
           (list 'problem* keep)
           (list 'proof (count parse-env 'env))
           (list 'proof* keep) 
           (list 'proposition (count parse-env 'env))
           (list 'proposition* keep)
           (list 'question (count parse-env 'env))
           (list 'question* keep)
           (list 'remark (count parse-env 'env))
           (list 'remark* keep)
           (list 'solution (count parse-env 'env))
           (list 'solution* keep)
           (list 'theorem (count parse-env 'env))
           (list 'theorem* keep)
           (list 'warning (count parse-env 'env))
           (list 'warning* keep)
           (list 'dueto keep)
           (list 'math parse-math)
           (list 'equation (count parse-math 'equation))
           (list 'equation* parse-math)
           (list 'eqnarray (count parse-math 'equation))
           (list 'eqnarray* parse-math)
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
           (list 'hlink keep-verbatim)
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
           
           ;; tm-doc style
           (list 'menu (parse-menu 0))
           (list 'submenu (parse-menu 1))
           (list 'subsubmenu (parse-menu 2))
           (list 'subsubsubmenu (parse-menu 3))
           (list 'subsubsubsubmenu (parse-menu 4))
           (list 'markup (change-to 'tt))
           (list 'explain-macro keep)
           (list 'src-var skip)
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
               (skip x))))
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
