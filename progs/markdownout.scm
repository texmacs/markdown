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
  (:use (convert tools output) (markdown-utils)))

(use-modules ((ice-9 regex)
              :select (string-match match:substring))
             ((srfi srfi-19)
              :select (string->date date->string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:flavour") "hugo"))

(define (author-by)
  (string-append (md-translate "By") ":"))

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
        (string-append
         "---"
         (serialize-yaml `(dict ,@(assoc->list (ahash-table->list front))))
         "\n---\n"))
      ""))

(define (postlude-add x)
  (cond ((func? x 'footnote)
         (md-set 'postlude
           (string-concatenate
            `(,(md-get 'postlude)
               "\n[^" ,(number->string (md-get 'footnote-nr)) "]: "
               ,@(md-map serialize-markdown* (cdr x))
               "\n"))))
        ((string? x)
         (md-set 'postlude (string-append (md-get 'postlude) "\n" x)))
        (else 
          (debug-message "convert-error" "postlude-add: bogus input")
          (noop))))

(define (postlude)
  (md-get 'postlude))

(define (indent-increment sn)
  "Increments indentation either by a number of spaces or a fixed string"
  (string-append (md-get 'indent)
    (if (number? sn) (string-concatenate (make-list sn " ")) sn)))

(define (indent-decrement n)
  (if (> (string-length (md-get 'indent)) n)
      (string-drop (md-get 'indent) n)
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown to string serializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep x)
  (cons (car x) (md-map serialize-markdown* (cdr x))))

(define (skip x)
  (string-concatenate (md-map serialize-markdown* (cdr x))))

(define (md-markdown x)
  (if (tm-is? x 'markdown)
      (serialize-markdown* (cdr x))
      (begin 
        (debug-message "convert-error" "Invalid markdown tree representation")
        "")))

(define (md-translate x)
  (cond ((null? x) "")
        ((string? x)
         (translate-from-to x "english" (md-get 'language)))
        ((func? x 'localize) (md-translate (cadr x)))
        ((list? x) (serialize-markdown* x))
        (else x)))

(define (md-doc-title x)
  (with title (serialize-markdown* (cdr x))
    (if (hugo-extensions?)
        (serialize-markdown* `(hugo-front "title" ,title))
        (serialize-markdown* `(document ,title "")))))

(define (md-doc-subtitle x)
  (with subtitle (serialize-markdown* (cdr x))
    (if (hugo-extensions?)
        (serialize-markdown* `(hugo-front "subtitle" ,subtitle))
        (serialize-markdown* `(document ,subtitle)))))

(define (md-doc-author x)
  ; TODO? We might want to extract other info
  (with name (select x '(:* author-name))
    (if (nnull? name)
        (if (hugo-extensions?)
            (md-set 'doc-authors (cons (cadar name) (md-get 'doc-authors)))
            (string-append (author-by) (force-string (cdar name))))))
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
      (with-md-globals 'paragraph-width #f
        (with-md-globals 'disable-shortcodes #t
          (md-hugo-frontmatter
           `(hugo-front "summary" ,(serialize-markdown* (cdr x))))))
      (md-paragraph `(concat (strong "Abstract: ") (em ,(cdr x))))))

(define (must-adjust? t)
  (tm-in? t '(strong em tt strike math concat cite cite-detail 
              eqref reference figure hlink)))

(define (md-paragraph x)
  ;; FIXME: arguments of Hugo shortcodes shouldn't be split
  (with adjust
      (cut adjust-width
           <> (md-get 'paragraph-width) (md-get 'indent) (md-get 'first-indent))
     (cond ((string? x) (adjust x))
           ((must-adjust? x) (adjust (serialize-markdown* x)))
           (else (serialize-markdown* x)))))

(define (md-document x)
  (string-concatenate
   (list-intersperse (md-map md-paragraph (cdr x))
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
        (serialize-markdown* `(document ,@(cdr x))))
      (string-concatenate (md-map serialize-markdown* (cdr x)))))

(define (md-header n)
  (lambda (x)
    (with-md-globals 'num-line-breaks 0
      (string-concatenate
       `(,@(make-list n "#") " " ,@(md-map serialize-markdown* (cdr x)))))))

(define (md-para x)
  "TeXmacs <paragraph> tag"
  (serialize-markdown* `(concat (strong ,@(cdr x)) " ")))

(define (md-environment x style)
  (let* ((txt (md-translate (second x)))
         (extra (if (and (string? (third x)) (string-null? (third x))) ""
                    `(concat " " ,(third x))))
         (content (cdr (fourth x)))  ; content of inner 'document
         (tag `(strong (concat ,txt ,extra ":"))))
    (serialize-markdown*
     (if (list>1? content)
         `(document (concat ,tag " " (,style ,(car content)))
                    (em (document ,@(cdr content))))
         `(document (concat ,tag " " (,style ,(car content))))))))

(define (md-environment* x style)
  (md-environment (list (first x) (second x) "" (third x)) style))

(define (md-make-environment style)
  (lambda (x) (md-environment x style)))

(define (md-make-environment* style)
  (lambda (x) (md-environment* x style)))

(define (md-dueto x)
  (serialize-markdown*
   `(concat " " (strong (concat "(" ,(cadr x) ")")) " ")))

(define (md-math x . leave-newlines?)
 "Takes a latex stree @x, and returns a valid MathJax-compatible LaTeX string"
 ; Set line length for latex output
 (with save (output-set-line-length (or (md-get 'paragraph-width) 9999))
   (with ltx (serialize-latex (second x))
     (output-set-line-length save)
     (if (null? leave-newlines?)
         (string-replace ltx "\n" " ")
         ltx))))

(define (md-span content . args)
  (string-append
   "<span " 
   (string-recompose-space (md-map assoc->html-attr args)) 
   ">" 
   (serialize-markdown* content)
   "</span>"))

(define (create-label-link label . extra-attrs)
  (with clean-label (sanitize-selector label)
    (apply md-span "" (append `((id . ,clean-label)) extra-attrs))))

(define (create-equation-link ltx)
  "Returns an empty anchor for every label in the latex line"
  (with matches (string-match "\\label\\{([^}]+)\\}" ltx)
    (if (not matches) ""
      (create-label-link (match:substring matches 1) '(class . "tm-eqlabel")))))

(define (escape-md-symbols line)
  "Escapes special markdown chars at the beginning of lines"
  (with matches (string-match "^( *)([-+*>]|\\d\\.)(.*)$" line)
    (if (not matches) line
        (string-append (match:substring matches 1)
                       "\\"
                       (match:substring matches 2)
                       (match:substring matches 3)))))

(define (md-equation x)
  (let*  ((s (md-math x (number? (md-get 'paragraph-width))))
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
          (filter (lambda (x) (and (string? x) (not (string-null? x)))) (cdr x))
        (md-set 'refs (append (md-get 'refs) citations))
        (md-hugo-shortcode 
         (cons 'cite (map (lambda (s) `(#f . ,s)) citations))))))

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

(define (md-figure type . extra-args)
  (lambda (x)
    (let* ((args (assoc-extend (cdr x) extra-args))
           (name (assoc-default args 'name ""))
           (caption (assoc-default args 'caption ""))
           (body (assoc-default args 'body '())))
      (if (hugo-extensions?)
          (begin
            (set! args (assoc-remove-many args '(body name caption)))
            (set! args (assoc-append? args 'class (md-get 'html-class)))
            (md-hugo-shortcode `(,type ,@args)
                               `(document ,body (concat ,name ,caption))))
          (with content (if (assoc 'src args)
                            `(image ,(assoc-ref args 'src) ,body)
                            body)
            (serialize-markdown*
             `(document ,body (concat ,name ,caption))))))))

(define (md-footnote x)
  ; Input: (footnote (document [stuff here]))
  (md-set 'footnote-nr (+ 1 (md-get 'footnote-nr)))
  (with-md-globals 'num-line-breaks 0
    (with-md-globals 'indent ""
      (with-md-globals 'paragraph-width #f
        (postlude-add x)
        (string-append "[^" (number->string (md-get 'footnote-nr)) "]")))))

(define (md-todo x)
  (md-span (serialize-markdown* (cdr x)) `(class . "todo")))

(define (md-block x)
  (with-md-globals 'num-line-breaks 1
    (let ((syntax (second x))
          (backquotes (md-encoding->tm-encoding "```" (md-get 'file?))))
      (string-concatenate
       `(,backquotes ,syntax
         "\n"
         ,@(md-map serialize-markdown* (cddr x))
         "\n"
         ,backquotes)))))

(define (md-hugo-frontmatter x)
  (if (odd? (length (cdr x)))
      (debug-message "convert-error"
                     "ERROR: frontmatter tag must have even number of entries")
      (when (hugo-extensions?)
        (with set-pair! (lambda (kv)
                          (ahash-set! (md-get 'frontmatter) (car kv) (cdr kv)))
          (map set-pair! (list->assoc (cdr x))))))
  "")

(define (md-hugo-shortcode x . inner)
  (if (not (md-get 'disable-shortcodes))
      (let ((shortcode (symbol->string (car x)))
            (args (cdr x))
            (content (if (null? inner) ""
                         (string-append (serialize-markdown* (car inner))
                                        "{{</" (symbol->string (car x)) ">}}"))))
        (string-trim-both
         (string-append
          (string-recompose-space
          `("{{<" ,shortcode ,@(map assoc->html-attr args) ">}}"))
          content)))
      ""))

(define (md-toc x)
  (if (hugo-extensions?)
      "{{< toc >}}"
      "Table of contents not implemented for raw Markdown"))

(define (md-bibliography x)
  (if (hugo-extensions?) 
      (md-hugo-shortcode '(references))
      (md-style '(strong "Bibliography not implemented for raw Markdown"))))

(define (md-sidenote-sub x numbered?)
  (if (hugo-extensions?)
      (let ((numbered (if numbered? '((numbered . "numbered")) '()))
            (args (cdr x)))
        (md-hugo-shortcode
         (append `(sidenote (halign . ,(md-marginal-style (first args)))
                            (valign . ,(md-marginal-style (second args))))
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
          "<" (string-recompose (md-map serialize-markdown* (cdr x)) "|" ) ">"))))

(define (md-tmdoc-copyright x)
  (with args (cdr x)
    (serialize-markdown*
     `(concat "---\n" "(C) " ,(first args)
              " by " ,(string-recompose-comma (cdr args))))))

; FIXME: either use tm-define everywhere or change these two:

(tm-define (md-tabular x)
  (serialize-markdown* `(document "Tables not implemented for raw markdown")))

(tm-define (md-tabular x)
  (:require (== "html" (get-preference "texmacs->markdown:table-format")))
  (let ((opts '(("texmacs->html:css" . "on")
                ("texmacs->html:mathjax" . "on")
                ("texmacs->html:mathml" . "off")
                ("texmacs->html:images" . "on"))))
    (serialize-html (texmacs->html (maybe-rewrap-html-class (cdr x)) opts))))

(define (md-html-class x)
  (with-md-globals 'html-class (second x)
    (serialize-markdown* (third x))))

(define-macro (maybe-rewrap-html-class . body)
  "Wraps in 'html-class tag if we saw one earlier and discarded it"
  `(with cl (md-get 'html-class)
     (if (string-nnull? cl) 
         (list 'html-class cl ,@body)
         ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEPRECATED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (paper-author-add x)
  "PaperWhy extension DEPRECATED"
  (if (hugo-extensions?)
      (md-hugo-frontmatter `(hugo-front "paper-authors" (tuple ,@(cdr x))))
      ""))

(define (md-hugo-tags x)
  "hugo-tags DEPRECATED, use `(hugo-front tags `(tuple tag1 tag2 ...)) "
  (if (hugo-extensions?)
      (md-hugo-frontmatter `(hugo-front "tags" (tuple (cdr x))))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (serialize-markdown* x)
;   (display* "Serialize: " x "\n")
  (cond ((string? x) x)
        ((char? x) (char->string x))
        ((symbol? x) "")
        ((and (nnull? x) (symbol? (car x)))
         (with fun (ahash-ref serialize-hash (car x) skip)
           (fun x)))
        (else
          (string-concatenate (md-map serialize-markdown* x)))))

(define serialize-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l))
     (list (list 'identity skip)
           (list 'markdown md-markdown)
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
           (list 'std-env (md-make-environment 'em))
           (list 'std-env* (md-make-environment* 'em))
           (list 'plain-env (md-make-environment 'identity))
           (list 'plain-env* (md-make-environment* 'identity))
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
           (list 'html-class md-html-class)
           (list 'small-figure (md-figure 'tmfigure '(class . "small-figure")))
           (list 'big-figure
             (md-figure 'tmfigure
                        '(marginal-caption . #t) '(class . "big-figure")))
           (list 'wide-figure (md-figure 'tmfigure '(class . "wide-figure")))
           (list 'marginal-figure (md-figure 'sidefigure))
           (list 'small-table (md-figure 'tmfigure))
           (list 'big-table (md-figure 'tmfigure ))
           (list 'tabular md-tabular)
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
    `((file? . #t)
      (language . ,(get-document-language))
      (num-line-breaks . 2)
      (paragraph-width . ,(get-preference "texmacs->markdown:paragraph-width"))
      (first-indent . "")
      (disable-shortcodes . #f)
      (html-class . "")
      (indent . "")
      (item . "* ")
      (postlude . "\n")
      (footnote-nr . 0)
      (labels . ())
      (doc-authors . ())
      (refs . ())
      (frontmatter . ,frontmatter))))

(tm-define (serialize-markdown x)
  (with-global md-globals (list->ahash-table (globals-defaults))
    (md-string (serialize-markdown* x))))

(tm-define (serialize-markdown-document x)
  (with-global md-globals (list->ahash-table (globals-defaults))
    (with body (serialize-markdown* x)
      (md-string (string-append (prelude) body (postlude))))))
