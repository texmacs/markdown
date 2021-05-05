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

(texmacs-module (convert markdown markdownout)
  (:use (convert tools output)))

; CAREFUL: srfi-19 overwrites some functions (e.g. current-time).
; Things might break!!
(use-modules (ice-9 regex) (srfi srfi-19))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state for document serialization and config options
;; Usage is wrapped within a "with-global" in serialize-markdown-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define globals #f)

(define (globals-defaults)
  (with frontmatter (make-ahash-table)
    (ahash-set! frontmatter "draft" "true")
    (ahash-set! frontmatter "date" 
                (strftime "%Y-%m-%d"(localtime (time-second (current-time)))))
    `((file? . #t)
      (num-line-breaks . 2)
      (paragraph-width . ,(get-preference "texmacs->markdown:paragraph-width"))
      (indent . "") 
      (postlude . "")
      (footnote-nr . 0)
      (labels . ())
      (doc-authors . ())
      (refs . ())
      (frontmatter . ,frontmatter))))

(define (get what)
  (ahash-ref globals what))

(define (set what value)
  (ahash-set! globals what value))

(define-public-macro (with-globals var val . body)
  (let ((old (gensym)) (new (gensym)))
    `(let ((,old (get ,var)))
       (set ,var ,val)
       (let ((,new (begin ,@body)))
         (set ,var ,old)
         ,new))))

(define (first-indent)
  (get 'indent))

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:flavour") "hugo"))

(define author-by (string-append (translate "By") ": "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: does this make sense? We only convert if exporting to file.
; The idea is that "Copy to markdown" might already perform some internal
; conversion before sending us the stree, because weird chars appear.
; However if we don't do any conversion here, the copied text is still wrong
(define (tm-encoding->md-encoding x)
  (if (get 'file?) (string-convert x "Cork" "UTF-8") x))

(define (md-encoding->tm-encoding x)
  (if (get 'file?) (string-convert x "UTF-8" "Cork") x))

(define (string-recompose-space s)
  (string-recompose s " "))

(define (string-recompose-newline s)
  (string-recompose s "\n"))

(define (list->csv l)
  (string-recompose-comma (map string-quote l)))

(define (list->yaml l indent-num)
  (let* ((indent (make-string indent-num #\ ))
         (item->yaml
          (lambda (it)
            (string-append indent "- " (string-quote (force-string it))))))
  (string-recompose-newline (map item->yaml l))))

(define (frontmatter->yaml front)
  "WIP: we only accept scalars and lists as values for now"
  (let* ((bool? (cut in? <> '("false" "true" "False" "True")))  ; yikes...
         (keys<=? (lambda (a b) (string<=? (car a) (car b))))
         (process-value
          (lambda (x)
            (cond ((tm-is? x 'date) (second x))
                  ((tm-is? x 'tuple) 
                   (string-append "\n" (list->yaml (cdr x) 2)))
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

(define (indent-increment s)
  (string-append (get 'indent) s))

(define (indent-decrement n)
  (lambda () 
    (if (> (string-length (get 'indent)) n)
        (string-drop-right (get 'indent) n)
        "")))

(define (prelude)
  "Output Hugo frontmatter"
  (if (hugo-extensions?)
      (with front (get 'frontmatter)
        (when (nnull? (get 'doc-authors))
          (ahash-set! front "authors" 
                      `(tuple ,@(reverse (get 'doc-authors)))))
        (when (nnull? (get 'refs))
          (ahash-set! front "refs" 
                      `(tuple ,@(list-remove-duplicates (get 'refs)))))
        (string-append "---\n" (frontmatter->yaml front) "---\n\n"))
      ""))

(define (postlude-add x)
  (cond ((list? x) 
         (set 'postlude 
               (string-concatenate 
                `(,(get 'postlude)
                 "\n"
                 "\n[^" ,(number->string (get 'footnote-nr)) "]: "
                 ,@(map serialize-markdown* x)))))
        ((string? x)
         (set 'postlude (string-append (get 'postlude) "\n" x)))
        (else 
          (display* "postlude-add: bogus input " x "\n")
          (noop))))

(define (postlude)
  (get 'postlude))

(define (adjust-width s cols prefix first-prefix)
  (if (not (get 'paragraph-width))  ; set width to #f to disable adjustment
      (md-string (string-append prefix s))
      (let* ((l (map md-string (string-split s #\ ))) ;split words
             (c (string-length prefix))
             (line-len 0)
             (proc (lambda (w acc)
                     (set! line-len (+ line-len (string-length w) 1))
                     (if (> line-len cols)
                         (begin
                           (set! line-len (+ c (string-length w)))
                           (string-append acc "\n" prefix w " "))
                         (string-append acc w " ")))))
        (string-trim-right (list-fold proc first-prefix l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown to string serializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep x)
  (cons (car x) (map serialize-markdown* (cdr x))))

(define (skip x)
  (string-concatenate (map serialize-markdown* (cdr x))))

(define (md-string s)
  ;HACK: tm-encoding (Cork) does not have newlines, so we work around those
  (string-recompose-newline
   (map tm-encoding->md-encoding (string-split s #\newline))))

(define (must-adjust? t)
  (and (list>1? t)
       (in? (car t)
            '(strong em tt strike math concat cite cite-detail 
                     eqref reference figure hlink))))

(define (md-markdown x)
  (if (tm-is? x 'markdown)
      (serialize-markdown* (cdr x))
      (begin (display "Invalid markdown tree representation") "")))

(define (md-doc-title x)
  (with title (md-string (serialize-markdown* (cdr x)))
    (if (hugo-extensions?)
        (md-hugo-frontmatter `(hugo-front "title" ,title))
        title)))

(define (md-doc-subtitle x)
  (display "FIXME: append subtitle to title")
  (with subtitle (md-string (serialize-markdown* (cdr x)))
    (if (hugo-extensions?)
        (md-hugo-frontmatter `(hugo-front "subtitle" ,subtitle))
        subtitle)))

(define (md-doc-author x)
  ; TODO? We might want to extract other info
  (with name (select x '(:* author-name))
    (if (nnull? name)
        (if (hugo-extensions?)
            (set 'doc-authors (cons (cadar name) (get 'doc-authors)))
            (string-append author-by (force-string (cdar name)) ))))
  "")

(define (md-doc-date x)
  "FIXME: handle errors, other formats"
  (with date (date->string (string->date (cadr x) "~B~d~Y") "~Y-~m-~d")
    (if (hugo-extensions?)
        (md-hugo-frontmatter `(hugo-front "date" ,date))
        date)))

(define (md-abstract x)
  (if (hugo-extensions?)
      (md-hugo-frontmatter `(hugo-front "summary" ,(serialize-markdown* (cdr x))))
      (md-document (md-style `(em ,(cdr x))))))

(define (md-paragraph p)
  ;; FIXME: arguments of Hugo shortcodes shouldn't be split
  (with adjust
      (cut adjust-width <> (get 'paragraph-width) (get 'indent) (first-indent))
    (cond ((string? p) (adjust p))
          ((must-adjust? p) (adjust (serialize-markdown* p)))
          (else (serialize-markdown* p)))))

(define (md-document x)
  (string-concatenate
   (list-intersperse (map md-paragraph (cdr x))
                     (make-string (get 'num-line-breaks) #\newline))))

(define (md-concat x)
  ; HACK: labels in sections will typically look like
  ;    (concat (section "Section one") (label "section-one"))
  ; But this will include a span in the md header, which then e.g. Hugo's 
  ; .TableOfContents will copy to the TOC hence producing two identical ids
  ; in the document. So we split concats in two lines
  (if (and (>= (length x) 3)
           (tuple? (second x))
           (in? (car (second x)) '(h1 h2 h3)))
      (with-globals 'num-line-breaks 1
        (md-document `(document ,@(cdr x))))
      (string-concatenate (map serialize-markdown* (cdr x)))))

(define (md-header n)
  (lambda (x)
    (with-globals 'num-line-breaks 0
      (string-concatenate
       `(,@(make-list n "#") " " ,@(map serialize-markdown* (cdr x)))))))

(define (md-para x)
  "TeXmacs <paragraph> tag"
  (serialize-markdown* `(concat (strong ,@(cdr x)) " ")))

(define (md-environment x)
  (let* ((txt (translate (string-capitalize (symbol->string (first x)))))
         (nr (second x))
         (tag `(strong ,(string-append txt " " nr ":")))
         (content (cdr (third x))))
    (serialize-markdown* 
     `(document (concat ,tag " " ,(car content)) ,@(cdr content)))))

(define (md-environment* x)
  (let* ((s (string-drop-right (symbol->string (car x)) 1))
         (txt (translate (string-capitalize s)))
         (tag `(strong ,(string-append txt ":")))
         (content (cdadr x)))
    (serialize-markdown* 
     `(document (concat ,tag " " ,(car content)) ,@(cdr content)))))

(define (md-dueto x)
  (serialize-markdown*
   `(concat " " (em (concat "(" ,(cadr x) ")")) " ")))

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
   "<span " (string-recompose-space (map process-attr args)) ">" 
   (serialize-markdown* content) "</span>")))

(define (create-label-link label)
  (md-span '() `("id" ,label)))

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
     (with-globals 'num-line-breaks 1
       (serialize-markdown* `(document ,@lines)))))

(define (md-numbered-equation x)
  (md-equation x))

(define (md-labels x)
  (set 'labels (list->ahash-table (cadr x)))
  "")

(define (md-label x)
  (create-label-link (serialize-markdown* (cadr x))))

(define (md-eqref x)
  (let* ((label (serialize-markdown* (cadr x)))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-display (ahash-ref (get 'labels) label err-msg)))
    (serialize-markdown*
      `(hlink ,(string-append "(" label-display ")") 
              ,(string-append "#" label)))))

(define (md-reference x)
  (let* ((label (serialize-markdown* (cadr x)))
         (err-msg (string-append "undefined label: '" label "'"))
         (label-display (ahash-ref (get 'labels) label err-msg)))
    (serialize-markdown*
     `(hlink ,label-display ,(string-append "#" label)))))

(define (md-item? x)
  (and (list>0? x) (func? x 'concat) (== (cadr x) '(item))))

(define (md-list x)
  (let* ((c (cond ((== (car x) 'itemize) "* ")
                  ((== (car x) 'enumerate) "1. ")
                  (else "* ")))
         (cs (string-concatenate (make-list (string-length c) " ")))
         (transform
          (lambda (a)
            (if (md-item? a) `(concat ,c ,@(cddr a)) a)))
         (doc (cAr x)))
    (with-globals 'num-line-breaks 1
      (with-globals 'indent (indent-increment cs)
        (with-globals 'first-indent (indent-decrement (string-length c))
          (serialize-markdown* `(document ,@(map transform (cdr doc)))))))))

(define (md-quotation x)
  (with-globals 'num-line-breaks 1
    (with-globals 'indent (indent-increment "> ")
      (serialize-markdown* (cAr x)))))

(define (md-style-text style)
 (cond ((== style 'strong) "**")
       ((== style 'em) "*")
       ((== style 'tt) (md-encoding->tm-encoding "`"))
       ((== style 'strike) "~~")
       ; TODO: Hugo shortcode?
       ((== style 'underline ""))
       (else "")))

(define (md-style x)
  (let* ((st (md-style-text (car x)))
         (content (string-concatenate (map serialize-markdown* (cdr x))))
         (whitespace-left? (string-starts? content " "))
         (whitespace-right? (string-ends? content " ")))
      (string-concatenate
       (list (if whitespace-left? " " "")
             st (string-trim-spaces content) st
             (if whitespace-right? " " "")))))

(define (md-cite x)
  "Custom hugo {{<cite>}} shortcode"
  (if (not (hugo-extensions?)) ""
      (with citations 
          (map force-string
               (filter (lambda (x) (and (string? x) (not (string-null? x))))
                       (cdr x)))
        (set 'refs (append (get 'refs) citations))
        (string-append
         "{{< cite " 
         (string-recompose-space (map string-quote citations))
         " >}}"))))

(define (md-cite-detail x)
  (if (not (hugo-extensions?)) ""
      (with detail (serialize-markdown* (cddr x))
        (string-append (md-cite `(cite ,(cadr x))) " (" detail ")"))))

(define (md-hlink x)
  (with payload (cdr x)
    (string-append "[" (serialize-markdown* (first payload)) "]"
                   "(" (force-string (second payload)) ")")))    

(define (md-image x)
  (with payload (cdr x)
      (string-append "![](" (force-string (first payload)) ")")))

(define (md-figure x)
  "Hugo {{< figure >}} shortcode"
  (if (not (hugo-extensions?)) ""
      (with payload (cdr x)
        (with-globals 'num-line-breaks 0
          (string-concatenate
           `("{{< figure src=" ,(string-quote (car payload))
             " title=" ,(string-quote 
                            (string-concatenate 
                             (map serialize-markdown* (cdr payload))))
             " >}}"))))))

(define (md-footnote x)
  ; Input: (footnote (document [stuff here]))
  (set 'footnote-nr (+ 1 (get 'footnote-nr)))
  (with-globals 'num-line-breaks 0
    (with-globals 'indent ""
      (with-globals 'paragraph-width #f
        (postlude-add (cdr x))
        (string-append "[^" (number->string (get 'footnote-nr)) "]")))))

(define (md-todo x)
  (md-span (serialize-markdown* (cdr x)) `("class" "todo")))

(define (md-block x)
  (with-globals 'num-line-breaks 1
    (with syntax (tm-ref x 0)
      (string-concatenate 
       `("```" ,syntax "\n" ,@(map serialize-markdown* (cddr x)) "```\n")))))

(define (md-hugo-frontmatter x)
  (when (hugo-extensions?)
    (with set-pair! (lambda (kv) 
                      (ahash-set! (get 'frontmatter) (car kv) (cdr kv)))
      (map set-pair! (list->assoc (cdr x)))))
  "")

(define (md-hugo-shortcode x)
  (when (hugo-extensions?)
    (string-recompose-space `("{{<" ,(cadr x) ,@(cddr x) ">}}"))))

(define (md-toc x)
  (if (hugo-extensions?)
      "{{< toc >}}"
      "Table of contents not implemented for raw Markdown"))

(define (md-bibliography x)
  (if (hugo-extensions?) 
      (md-hugo-shortcode '(_ "references")) 
      (md-style '(strong "Bibliography not implemented"))))

(define (md-sidenote x)
  (if (hugo-extensions?)
      (with styles
          (list->ahash-table '(("b" . "bottom") ("c" . "center") ("t" . "top")
                               ("normal" . "right")))
       (with args (cdr x)
         (string-append "{{< sidenote "
                        "halign=" (string-quote (ahash-ref styles (first args)))
                        " "
                        "valign=" (string-quote (ahash-ref styles (second args)))
                        " >}}"
                        (serialize-markdown* (third args))
                        "{{</ sidenote >}}"))
        "")))

(define (serialize-markdown* x)
  ;(display* "Serialize: " x "\n")
  (cond ((null? x) "")
        ((string? x) x)
        ((char? x) (char->string x))
        ((symbol? x) "")
        ((symbol? (car x))
         (with fun (ahash-ref serialize-hash (car x) skip)
           (fun x)))
        (else
         (string-concatenate (map serialize-markdown* x)))))

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

(define serialize-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l)) 
     (list (list 'markdown md-markdown)
           (list 'labels md-labels)
           (list 'strong md-style)
           (list 'em md-style)
           (list 'tt md-style)
           (list 'strike md-style)
           (list 'block md-block)
           (list 'document md-document)         
           (list 'acknowledgments md-environment)
           (list 'acknowledgments* md-environment*)
           (list 'algorithm md-environment)
           (list 'algorithm* md-environment*)
           (list 'answer md-environment)
           (list 'answer* md-environment*)
           (list 'axiom md-environment)
           (list 'axiom* md-environment*)
           (list 'conjecture md-environment)
           (list 'conjecture* md-environment*)
           (list 'convention md-environment)
           (list 'convention* md-environment*)
           (list 'corollary md-environment)
           (list 'corollary* md-environment*)
           (list 'definition md-environment)
           (list 'definition* md-environment*)
           (list 'example md-environment)
           (list 'example* md-environment*)
           (list 'exercise md-environment)
           (list 'exercise* md-environment*)
           (list 'lemma md-environment)
           (list 'lemma* md-environment*)
           (list 'notation md-environment)
           (list 'notation* md-environment*)
           (list 'problem md-environment)
           (list 'problem* md-environment*)
           (list 'proof md-environment)
           (list 'proof* md-environment*) 
           (list 'proposition md-environment)
           (list 'proposition* md-environment*)
           (list 'question md-environment)
           (list 'question* md-environment*)
           (list 'remark md-environment)
           (list 'remark* md-environment*)
           (list 'solution md-environment)
           (list 'solution* md-environment*)
           (list 'theorem md-environment)
           (list 'theorem* md-environment*)
           (list 'warning md-environment)
           (list 'warning* md-environment*)
           (list 'dueto md-dueto)
           (list 'math md-math)
           (list 'equation md-numbered-equation)
           (list 'equation* md-equation)
           (list 'eqnarray md-numbered-equation)
           (list 'eqnarray* md-equation)
           (list 'concat md-concat)
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
           (list 'figure md-figure)
           (list 'hlink md-hlink)
           (list 'tags md-hugo-tags)  ; Hugo extension (DEPRECATED)
           (list 'hugo-short md-hugo-shortcode)  ; Hugo extension
           (list 'hugo-front md-hugo-frontmatter)  ; Hugo extension
           (list 'table-of-contents md-toc) ; Hugo extension
           (list 'bibliography md-bibliography)
           (list 'marginal-note md-sidenote) ; TfL extension
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (serialize-markdown x)
  (with-global globals (list->ahash-table (globals-defaults))
    (serialize-markdown* x)))

(tm-define (serialize-markdown-document x)
  (with-global globals (list->ahash-table (globals-defaults))
    (with body (serialize-markdown* x)
      (string-append (prelude) body (postlude)))))
