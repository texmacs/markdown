;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdownout.scm
;; DESCRIPTION : markdown-stree to markdown-document or markdown-snippet
;; COPYRIGHT   : (C) 2017 Ana Cañizares García and Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (markdownout)
  (:use (convert tools output) (utils)))

; CAREFUL: srfi-19 overwrites some functions (e.g. current-time).
; Things might break!!
(use-modules (ice-9 regex) (srfi srfi-19))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:flavour") "hugo"))

(define (author-by)
  (string-append (md-translate "By") ":"))

(define (frontmatter->yaml front)
  "Note: this only accepts scalars and lists as values for now"
  (let* ((bool? (cut in? <> '("false" "true" "False" "True")))  ; yikes...
         (keys<=? (lambda (a b) (string<=? (car a) (car b))))
         (process-value
          (lambda (x)
            (cond ((tm-is? x 'date) (second x))
                  ((tm-is? x 'tuple) 
                   (string-append "\n" (list->yaml (list-sort (cdr x) string<=?) 2)))
                  ((tuple? x) (string-append "\n" (list->yaml x 2)))
                  ((bool? x) x)
                  ((string? x) (string-quote x)))))  ; quote everything else
         (process-key-value
          (lambda (x)
            (if (npair? x) ""
              (string-append (car x) ": " (process-value (cdr x)))))))
      (string-append 
       (string-recompose-newline 
        (map process-key-value (list-sort (ahash-table->list front) keys<=?)))
        "\n")))

(define (indent-increment sn)
  "Increments indentation either by a number of spaces or a fixed string"
  (string-append (md-get 'indent)
    (if (number? sn) (string-concatenate (make-list sn " ")) sn)))

(define (indent-decrement n)
  (if (> (string-length (md-get 'indent)) n)
      (string-drop (md-get 'indent) n)
      ""))

(define (prelude)
  "Output Hugo frontmatter"
  (if (hugo-extensions?)
      (with front (md-get 'frontmatter)
        (when (nnull? (md-get 'doc-authors))
          (ahash-set! front "authors" 
                      `(tuple ,@(reverse (md-get 'doc-authors)))))
        (when (nnull? (md-get 'refs))
          (ahash-set! front "refs" 
                      `(tuple ,@(list-remove-duplicates (md-get 'refs)))))
        (string-append "---\n" (frontmatter->yaml front) "---\n\n"))
      ""))

(define (postlude-add x)
  (cond ((list? x) 
         (md-set 'postlude 
               (string-concatenate 
                `(,(md-get 'postlude)
                 "\n"
                 "\n[^" ,(number->string (md-get 'footnote-nr)) "]: "
                 ,@(map serialize-markdown* x)))))
        ((string? x)
         (md-set 'postlude (string-append (md-get 'postlude) "\n" x)))
        (else 
          (display* "postlude-add: bogus input " x "\n")
          (noop))))

(define (postlude)
  (md-get 'postlude))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown to string serializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep x)
  (cons (car x) (map serialize-markdown* (cdr x))))

(define (skip x)
  (string-concatenate (map serialize-markdown* (cdr x))))

(define (must-adjust? t)
  (and (list>1? t)
       (in? (car t)
            '(strong em tt strike math concat cite cite-detail 
                     eqref reference figure hlink))))

(define (md-markdown x)
  (if (tm-is? x 'markdown)
      (serialize-markdown* (cdr x))
      (begin (display "Invalid markdown tree representation") "")))

(define (md-translate x)
  (cond ((null? x) "")
        ((string? x)
         (translate-from-to x "english" (md-get 'language)))
        ((func? x 'localize) (md-translate (cadr x)))
        ((list? x) (serialize-markdown* x))
        (else x)))

(define (md-doc-title x)
  (with title (md-string (serialize-markdown* (cdr x)))
    (if (hugo-extensions?)
        (serialize-markdown* `(hugo-front "title" ,title))
        (serialize-markdown* `(document ,title "")))))

(define (md-doc-subtitle x)
  (with subtitle (md-string (serialize-markdown* (cdr x)))
    (if (hugo-extensions?)
        (serialize-markdown* `(hugo-front "subtitle" ,subtitle))
        (serialize-markdown* `(document ,subtitle)))))

(define (md-doc-author x)
  ; TODO? We might want to extract other info
  (with name (select x '(:* author-name))
    (if (nnull? name)
        (if (hugo-extensions?)
            (md-set 'doc-authors (cons (cadar name) (md-get 'doc-authors)))
            (string-append author-by (force-string (cdar name)) ))))
  "")

(define (decode-date date formats)
  (cond ((nlist? formats) (decode-date date (list formats)))
        ((null? formats) (string-append "Failed to convert date: "
                                        (force-string date)))
        (else (catch #t
                     (lambda () (date->string 
                                 (string->date date (car formats))
                                 "~Y-~m-~d"))
                     (lambda _ (decode-date date (cdr formats)))))))

(define (md-doc-date x)
  (with date (decode-date (cadr x) '("~B~d~Y" "~d~B~Y"))
    (if (hugo-extensions?)
        (md-hugo-frontmatter `(hugo-front "date" ,date))
        date)))

(define (md-abstract x)
  (if (hugo-extensions?)
      (md-hugo-frontmatter `(hugo-front "summary" ,(serialize-markdown* (cdr x))))
      (md-paragraph `(concat (strong "Abstract: ") (em ,(cdr x))))))

(define (md-paragraph p)
  ;; FIXME: arguments of Hugo shortcodes shouldn't be split
  (with adjust
      (cut adjust-width
           <> (md-get 'paragraph-width) (md-get 'indent) (md-get 'first-indent))
     (cond ((string? p) (adjust p))
           ((must-adjust? p) (adjust (serialize-markdown* p)))
           (else (serialize-markdown* p)))))

(define (md-document x)
  (string-concatenate
   (list-intersperse (map md-paragraph (cdr x))
                     (make-string (md-get 'num-line-breaks) #\newline))))

(define (md-concat x)
  ; HACK: labels in sections will typically look like
  ;    (concat (section "Section one") (label "section-one"))
  ; But this will include a span in the md header, which then e.g. Hugo's 
  ; .TableOfContents will copy to the TOC hence producing two identical ids
  ; in the document. So we split concats in two lines
  (if (and (>= (length x) 3)
           (tuple? (second x))
           (in? (car (second x)) '(h1 h2 h3)))
      (with-md-globals 'num-line-breaks 1
        (md-document `(document ,@(cdr x))))
      (string-concatenate (map serialize-markdown* (cdr x)))))

(define (md-header n)
  (lambda (x)
    (with-md-globals 'num-line-breaks 0
      (string-concatenate
       `(,@(make-list n "#") " " ,@(map serialize-markdown* (cdr x)))))))

(define (md-para x)
  "TeXmacs <paragraph> tag"
  (serialize-markdown* `(concat (strong ,@(cdr x)) " ")))

(define (md-environment x)
  (let* ((txt (md-translate (second x)))
         (extra (if (and (string? (third x)) (string-null? (third x))) ""
                    `(concat " " ,(third x))))
         (content (cdr (fourth x)))  ; content of inner 'document
         (tag `(strong (concat ,txt ,extra ":"))))
    (serialize-markdown*
     (if (list>1? content)
         `(document (concat ,tag " " (em ,(car content)))
                    (em (document ,@(cdr content))))
         `(document (concat ,tag " " (em ,(car content))))))))

(define (md-environment* x)
  (md-environment (list (first x) (second x) "" (third x))))

(define (md-dueto x)
  (serialize-markdown*
   `(concat " " (strong (concat "(" ,(cadr x) ")")) " ")))

(define (md-math x . leave-newlines?)
 "Takes a latex stree @x, and returns a valid MathJax-compatible LaTeX string"
 (with ltx (serialize-latex (second x))
   (if (null? leave-newlines?)
       (string-replace ltx "\n" " ")
       ltx)))

(define (md-span content . args)
  (with process-attr 
      (lambda (x) 
        (string-append (first x) "=" (string-quote (second x))))
    (string-append
      "<span " 
      (string-recompose-space (map process-attr args)) 
      ">" 
      (serialize-markdown* content)
      "</span>")))

(define (create-label-link label)
  (with clean-label (sanitize-selector label)
    (md-span '() `("id" ,clean-label))))

(define (create-equation-link ltx)
  "Returns an empty anchor for every label in the latex line"
  (with matches (string-match "\\label\\{([^}]+)\\}" ltx)
    (if (not matches) ""
      (create-label-link (match:substring matches 1)))))

(define (escape-md-symbols line)
  "Escapes special markdown chars at the beginning of lines"
  (with matches (string-match "^( *)([-+*>]|\\d\\.)(.*)$" line)
    (if (not matches) line
        (string-append (match:substring matches 1)
                       "\\"
                       (match:substring matches 2)
                       (match:substring matches 3)))))

(define (md-equation x)
  ;; HACK
  (let*  ((s (md-math x #t))
          (s1 (string-replace s "\\[" "\\\\["))
          (s2 (string-replace s1 "\\]" "\\\\]"))
          (s3 (string-split s2 #\newline))
          (s4 (map escape-md-symbols s3))
          (anchors (string-concatenate (map create-equation-link s4)))
          (lines (if (string-null? anchors) s4 (cons anchors s4))))
     (with-md-globals 'num-line-breaks 1
       (serialize-markdown* `(document ,@lines)))))

(define (md-numbered-equation x)
  (md-equation x))

(define (md-labels x)
  (md-set 'labels (list->ahash-table (cadr x)))
  "")

(define (md-label x)
  (create-label-link (serialize-markdown* (cadr x))))

(define (md-eqref x)
  (let* ((label (serialize-markdown* (cadr x)))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-display (ahash-ref (md-get 'labels) label err-msg)))
    (serialize-markdown*
      `(hlink ,(string-append "(" label-display ")") 
              ,(string-append "#" label)))))

(define (md-reference x)
  (let* ((label (serialize-markdown* (cadr x)))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-display (ahash-ref (md-get 'labels) label err-msg)))
    (serialize-markdown*
     `(hlink ,label-display ,(string-append "#" label)))))

(define (md-item x)
  (md-get 'item))

(define (is-item? x)
  (nnull? (select x '(:%0 item))))

(define (is-item-subparagraph? x)
  "#t if @x is a (text) subparagraph of an item. Excludes subitemizes and others."
  (not (or (symbol? x)
           (stree-contains? x '(itemize enumerate quotation item)))))

(define (add-paragraphs-after-items l indent)
  "Adds empty lines in items with multiple paragraphs"
  ; paragraphs inside an itemize but don't begin with an (item) are
  ; considered part of the previous item.
  (with transform
      (lambda (x acc)
        (append acc (if (is-item-subparagraph? x)
                        (list "" `(concat ,indent ,x))
                        (list x))))
  (list-fold transform '() l)))

(define (md-list x)
  (let ((c (cond ((== (car x) 'itemize) "* ")
                 ((== (car x) 'enumerate) "1. ")
                 (else "* "))))
    (with-md-globals 'num-line-breaks 1
      (with-md-globals 'item c
        (with-md-globals 'indent (indent-increment (string-length c))
          (with-md-globals 'first-indent (indent-decrement (string-length c))
;             (display* "indent = " (string-length (md-get 'indent))
;                       ", first indent = " (string-length (md-get 'first-indent))
;                       "\n\n")
            (serialize-markdown*
             (add-paragraphs-after-items
              (cadr x)
              (string-concatenate (make-list (string-length c) " "))))))))))

(define (md-quotation x)
  (with-md-globals 'num-line-breaks 1
    (with-md-globals 'indent (indent-increment "> ")
      (with-md-globals 'first-indent (md-get 'indent)
        (serialize-markdown* (cdr x))))))

(define (md-style-text style)
 (cond ((== style 'strong) "**")
       ((== style 'em) "*")
       ((== style 'tt) (md-encoding->tm-encoding "`" (md-get 'file?)))
       ((== style 'strike) "~~")
       ; TODO: Hugo shortcode?
       ((== style 'underline ""))
       (else "")))

(define (md-style-inner st x)
;   (display* "++++ inner: " x "\n")
  (let* ((style (md-style-text st))
         (content* (serialize-markdown* x))
         (left (if (string-starts? content* " ") " " ""))
         (right (if (string-ends? content* " ") " " ""))
         (content (string-trim-spaces content*)))
    (cond ((string-null? content) "")
          ((string-punctuation? content) content)
          (else 
            (string-concatenate
             (list left style content style right))))))

;;;;;;;;;;;;;;
; FIXME: Move this style preprocessing to tmmarkdown
; Besides it making more sense there, it might be necessary to have idempotent
; styles (i.e. em of em is no style)

(define md-style-tag-list '(em strong tt strike underline))
(define md-style-drop-tag-list
  '(marginal-note marginal-note* footnote footnote* label item
    equation equation* eqnarray eqnarray* math))
(define md-stylable-tag-list '(document itemize enumerate theorem ))  ;FIXME

(define (add-style-to st x)
  "Recurses into children of @x inserting its tag where necessary."
  (cond ((string? x)
         `(,st ,x))
        ((== st (first x))  ; UNTESTED: drop repeated styles
         (add-style-to st (cadr x)))
        ((tm-in? x md-stylable-tag-list)
         `(,(first x) ,@(map (cut add-style-to st <>) (cdr x))))
        ((and (tm-in? x md-style-tag-list) 
              (not (tm-in? (second x) md-style-tag-list)))
         `(,(first x) ,@(map (cut add-style-to st <>) (cdr x))))
        ((tm-in? x md-style-drop-tag-list)
         x)
        ((is-item? x)
         `(concat ,@(map (cut add-style-to st <>) (cdr x))))
        (else
          `(,st ,x))))

(define (md-style x)
  (let* ((st (car x))
         (content (cadr x)))
    (cond ((tm-in? content md-stylable-tag-list)
           (with styled (add-style-to st content)
             (serialize-markdown* styled)))
          ((tm-in? content md-style-drop-tag-list)
           (serialize-markdown* content))
          (else
            (md-style-inner st content)))))

(define (md-cite x)
  "Custom hugo {{<cite>}} shortcode"
  (if (not (hugo-extensions?)) ""
      (with citations 
          (map force-string
               (filter (lambda (x) (and (string? x) (not (string-null? x))))
                       (cdr x)))
        (md-set 'refs (append (md-get 'refs) citations))
        (md-hugo-shortcode (cons 'cite citations)))))

(define (md-cite-detail x)
  (if (not (hugo-extensions?)) ""
      (with detail (serialize-markdown* (cddr x))
        (string-append (md-cite `(cite ,(cadr x))) " (" detail ")"))))

(define (md-hlink x)
  (with payload (cdr x)
    (string-append "[" (serialize-markdown* (first payload)) "]"
                   "(" (force-string (second payload)) ")")))    

(define (md-image x)
  (let* ((payload (cdr x))
         (src (first payload))
         (alt (if (list-2? payload) (second payload) "")))
    (string-append "![" alt "](" (force-string src) ")")))

; FIXME: clear this mess with figures, don't expect img as content, etc.
(define (md-figure-sub payload)
  (let* ((src (force-string (car payload)))
         (title
           (with-md-globals 'num-line-breaks 0 ; Don't break lines in 'document
             (string-concatenate (map serialize-markdown* (cdr payload))))))
    (list src title)))

(define (md-figure type . args)
  ;; (display* "md-figure: " args "\n")
  (lambda (x)
    ;(display* "md-figure captured: " args "\n")
    (with params (md-figure-sub (cdr x))
      (if (hugo-extensions?)
          (md-hugo-shortcode `(,type (src ,(car params)) ,@args) (cadr params))
          (md-image (list 'image (car params) (cadr params)))))))

(define (md-marginal-figure type . args)
  (lambda (x)
    (let ((params (md-figure-sub (cddr x)))
          (vpos (cadr x)))
      (if (hugo-extensions?)
          (md-hugo-shortcode `(,type (valign ,(marginal-style vpos))
                                     (src ,(car params))
                                     ,@args)
                             (cadr params))
          (md-image (list 'image (car params) (cadr params)))))))


(define (md-footnote x)
  ; Input: (footnote (document [stuff here]))
  (md-set 'footnote-nr (+ 1 (md-get 'footnote-nr)))
  (with-md-globals 'num-line-breaks 0
    (with-md-globals 'indent ""
      (with-md-globals 'paragraph-width #f
        (postlude-add (cdr x))
        (string-append "[^" (number->string (md-get 'footnote-nr)) "]")))))

(define (md-todo x)
  (md-span (serialize-markdown* (cdr x)) `("class" "todo")))

(define (md-block x)
  (with-md-globals 'num-line-breaks 1
    (with syntax (tm-ref x 0)
      (string-concatenate 
       `("```" ,syntax "\n" ,@(map serialize-markdown* (cddr x)) "\n```")))))

(define (md-hugo-frontmatter x)
  (if (odd? (length (cdr x)))
      (display* "ERROR: frontmatter tag must have even number of entries")
      (when (hugo-extensions?)
        (with set-pair! (lambda (kv)
                          (ahash-set! (md-get 'frontmatter) (car kv) (cdr kv)))
          (map set-pair! (list->assoc (cdr x))))))
  "")

(define (md-hugo-shortcode x . inner)
  (when (hugo-extensions?)
    (letrec
        ((process-one
          (lambda (arg)
            (cond ((list-2? arg)
                    (string-append (process-one (first arg))
                                   "=" (process-one (second arg))))
                   ((list-1? arg) (string-quote (car arg)))
                   ((symbol? arg) (symbol->string arg))
                   ((string? arg) (string-quote arg))
                   ((boolean? arg) (string-quote (if arg "true" "false")))
                   (else ""))))
          (shortcode (symbol->string (car x)))
          (arguments (cdr x))
          (content (if (null? inner) ""
                       (string-append (serialize-markdown* inner)
                                      "{{</" (symbol->string (car x)) ">}}"))))
      (string-trim-both
        (string-append
          (string-recompose-space
            `("{{<" ,shortcode ,@(map process-one arguments) ">}}"))
          content)))))

(define (md-toc x)
  (if (hugo-extensions?)
      "{{< toc >}}"
      "Table of contents not implemented for raw Markdown"))

(define (md-bibliography x)
  (if (hugo-extensions?) 
      (md-hugo-shortcode '(references))
      (md-style '(strong "Bibliography not implemented for raw Markdown"))))

(define marginal-styles-table
  (list->ahash-table '(("b" . "bottom") ("c" . "center")
                       ("t" . "top") ("normal" . "right"))))

(define (marginal-style s)
  (ahash-ref marginal-styles-table s))

(define (md-sidenote-sub x numbered?)
  (if (hugo-extensions?)
      (let ((numbered (if numbered? '((numbered "numbered")) '()))
            (args (cdr x)))
        (md-hugo-shortcode
         (append `(sidenote (halign ,(marginal-style (first args)))
                            (valign ,(marginal-style (second args))))
                 numbered)
         (third args)))
      (serialize-markdown* `(footnote ,(third (cdr x))))))

(define (md-sidenote x)
  (md-sidenote-sub x #t))

(define (md-sidenote* x)
  (md-sidenote-sub x #f))

(define (md-explain-macro x)
  ; FIXME: this will break with nested macros (tt style will be interrupted)
  (md-style
   `(tt ,(string-append 
          "<" (string-recompose (map serialize-markdown* (cdr x)) "|" ) ">"))))

(define (md-tmdoc-copyright x)
  (with args (cdr x)
    (serialize-markdown*
     `(concat "---\n" "(C) " ,(first args)
              " by " ,(string-recompose-comma (cdr args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPRECATED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (paper-author-add x)
  "PaperWhy extension DEPRECATED"
  (if (hugo-extensions?)
      (md-hugo-frontmatter '(hugo-front "paper-authors" (cdr x)))
      ""))

(define (md-hugo-tags x)
  "hugo-tags DEPRECATED, use `(hugo-front tags `(tuple tag1 tag2 ...)) "
  (if (hugo-extensions?)
      (md-hugo-frontmatter '(hugo-front "tags" (cdr x)))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (serialize-markdown* x)
;   (display* "Serialize: " x "\n")
  (cond ((null? x) "")
        ((string? x) x)
        ((char? x) (char->string x))
        ((symbol? x) "")
        ((symbol? (car x))
         (with fun (ahash-ref serialize-hash (car x) skip)
           (fun x)))
        (else
         (string-concatenate (map serialize-markdown* x)))))

(define serialize-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l))
     (list (list 'markdown md-markdown)
           (list 'localize md-translate)
           (list 'labels md-labels)
           (list 'strong md-style)
           (list 'em md-style)
           (list 'tt md-style)
           (list 'strike md-style)
           (list 'underline md-style)
           (list 'block md-block)
           (list 'quotation md-quotation)
           (list 'document md-document)
           (list 'std-env md-environment)
           (list 'std-env* md-environment*)
           (list 'dueto md-dueto)
           (list 'math md-math)
           (list 'equation md-numbered-equation)
           (list 'equation* md-equation)
           (list 'eqnarray md-numbered-equation)
           (list 'eqnarray* md-equation)
           (list 'concat md-concat)
           (list 'item md-item)
           (list 'itemize md-list)
           (list 'enumerate md-list)
           (list 'h1 (md-header 1))
           (list 'h2 (md-header 2))
           (list 'h3 (md-header 3))
           (list 'para md-para)
           (list 'doc-date md-doc-date)
           (list 'doc-title md-doc-title)
           (list 'doc-subtitle md-doc-subtitle)
           (list 'doc-author md-doc-author)
           (list 'abstract md-abstract)
           (list 'paper-author-name paper-author-add)  ; Paperwhy extension
           (list 'cite md-cite)
           (list 'cite-detail md-cite-detail)
           (list 'eqref md-eqref)
           (list 'label md-label)
           (list 'reference md-reference)
           (list 'footnote md-footnote)
           (list 'todo md-todo)
           (list 'image md-image)
           (list 'small-figure (md-figure 'tmfigure))
           (list 'small-figure* (md-figure 'tmfigure))
           (list 'big-figure 
                 (md-figure 'tmfigure '(marginal-caption "true") '(width "100%")))
           (list 'big-figure* 
                 (md-figure 'tmfigure '(marginal-caption "true") '(width "100%")))
           (list 'wide-figure
                 (md-figure 'tmfigure '(class "wide-figure") '(width "100%")))
           (list 'wide-figure*
                 (md-figure 'tmfigure '(class "wide-figure") '(width "100%")))
           (list 'marginal-figure (md-marginal-figure 'sidefigure))
           (list 'marginal-figure* (md-marginal-figure 'sidefigure))
           (list 'hlink md-hlink)
           (list 'tags md-hugo-tags)  ; Hugo extension (DEPRECATED)
           (list 'hugo-short md-hugo-shortcode)  ; Hugo extension
           (list 'hugo-front md-hugo-frontmatter)  ; Hugo extension
           (list 'table-of-contents md-toc) ; Hugo extension
           (list 'bibliography md-bibliography)  ; TfL extension
           (list 'marginal-note md-sidenote) ; TfL extension
           (list 'marginal-note* md-sidenote*) ; TfL extension
           (list 'explain-macro md-explain-macro)
           (list 'tmdoc-copyright md-tmdoc-copyright)
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (globals-defaults)
  (with frontmatter (make-ahash-table)
    ;(ahash-set! frontmatter "draft" "true")
    ;(ahash-set! frontmatter "date" 
    ;            (strftime "%Y-%m-%d"(localtime (time-second (current-time)))))
    `((file? . #t)
      (language . ,(get-document-language))
      (num-line-breaks . 2)
      (paragraph-width . ,(get-preference "texmacs->markdown:paragraph-width"))
      (first-indent . "")
      (indent . "")
      (item . "* ")
      (postlude . "")
      (footnote-nr . 0)
      (labels . ())
      (doc-authors . ())
      (refs . ())
      (frontmatter . ,frontmatter))))

(tm-define (serialize-markdown x)
  (with-global md-globals (list->ahash-table (globals-defaults))
    (serialize-markdown* x)))

(tm-define (serialize-markdown-document x)
  (with-global md-globals (list->ahash-table (globals-defaults))
    (with body (serialize-markdown* x)
      (string-append (prelude) body (postlude)))))
