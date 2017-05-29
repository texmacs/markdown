;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmmarkdown.scm
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Global" state for document serialization and config options
;; Usage is wrapped within a "with-global" in serialize-markdown-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define footnote-nr 0)
(define equation-nr 0)  ; global counter for equations
(define num-line-breaks 2)
(define authors '())
(define doc-title "")
(define postlude "")
(define labels '())
(define indent "")
(define (first-indent) indent)

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:hugo-extensions") "on"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (indent-increment s)
  (string-append indent s))

(define (author-add x)
  (set! authors (append authors (cdr x)))
  (display* authors)
  "")

(define (indent-decrement n)
  (lambda () 
    (if (> (string-length indent) n)
        (string-drop-right indent n)
        "")))

(define (prelude)
  "Output Hugo frontmatter"
  (if (not (hugo-extensions?)) ""
      (let ((authors* (string-join
                      (map (lambda (x) (string-append "\"" x "\"")) authors)
                      ", "))
            (date (strftime "%Y-%m-%d"(localtime (current-time)))))
        (string-append "---\n\n"
                       "title: \"" doc-title "\"\n"
                       "date: \"" date "\"\n"
                       "authors: [" authors* "]\n"
                       "tags: [\"\"]\n"
                       "paper_authors: [\"\", \"\"]\n"
                       "paper_key: \"\"\n\n"
                       "---\n\n"))))

(define (postlude-add x)
  (cond ((list? x) 
         (set! postlude 
               (string-concatenate `(,postlude
                                     "\n[^" ,(number->string footnote-nr) "]: "
                                     ,@(map serialize-markdown x)))))
        ((string? x)
         (set! postlude (string-append postlude "\n" x)))
        (else 
          (display* "postlude-add: bogus input " x "\n")
          (noop))))

; There are probably a dozen functions in TeXmacs doing the 
; very same thing as these two...
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown to string serializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep x)
  (cons (car x) (map serialize-markdown (cdr x))))

(define (skip x)
  (string-concatenate (map serialize-markdown (cdr x))))

(define (md-string s)
  (cork->utf8 s))

(define (adjust-width s cols prefix first-prefix)
  (let* ((l (map md-string (string-split s #\ )))
         (c (string-length prefix))
         (line-len 0)
         (proc (lambda (w acc)
                 (set! line-len (+ line-len (string-length w) 1))
                 (if (> line-len cols)
                     (begin
                       (set! line-len (+ c (string-length w)))
                       (string-append acc "\n" prefix w " "))
                     (string-append acc w " ")))))
    (string-trim-right (list-fold proc first-prefix l))))

(define (md-paragraph p)
  (with pw (get-preference "texmacs->markdown:paragraph-width")
    (line-breaks-after
     (if (string? p)
         ; FIXME: arguments of Hugo shortcodes shouldn't be split
         (adjust-width p pw indent (first-indent))
         (serialize-markdown p)))))

 (define (md-paragraph p)
   (with pw (get-preference "texmacs->markdown:paragraph-width")
     (line-breaks-after     
      (cond ((string? p)
             ; FIXME: arguments of Hugo shortcodes shouldn't be split
             (adjust-width p pw indent (first-indent)))
            ((func? p 'concat)
             (adjust-width (serialize-markdown p) pw indent (first-indent)))
           (else 
            (serialize-markdown p))))))

(define (md-document x)
  (string-concatenate (map md-paragraph (cdr x))))

(define (md-concat x)
  (string-concatenate (map serialize-markdown (cdr x))))

(define (line-breaks-after s)
  (string-concatenate `(,s ,@(make-list num-line-breaks "\n"))))

(define (md-header n)
  (lambda (x)
    (with-global num-line-breaks 1
      (with res (string-concatenate
                 `(,@(make-list n "#")
                   " "
                   ,@(map serialize-markdown (cdr x))))
            (if (<= n 4)  ; Special handling of TeXmacs <paragraph>
                (line-breaks-after res)
                (string-append res " "))))))

(define (math->latex t)
 "Converts the TeXmacs tree @t into internal LaTeX representation"
 (with options '(("texmacs->latex:replace-style" . "on")
                 ("texmacs->latex:expand-macros" . "on")
                 ("texmacs->latex:expand-user-macros" . "off")
                 ("texmacs->latex:indirect-bib" . "off")
                 ("texmacs->latex:encoding" . "utf8")
                 ("texmacs->latex:use-macros" . "off"))
 (texmacs->latex t options)))

(define (md-environment x)
  (string-append 
   (md-style 
    `(strong 
      ,(translate 
        (string-capitalize (symbol->string (car x))))))
   ": "
   (string-concatenate (map serialize-markdown (cdr x)))))

(define (md-math* t)
  (replace-fun-list t
   `((mathbbm . mathbb)
     ((_) . "\\_")
     (,(cut func? <> '!sub) . 
       ,(lambda (x) (cons "\\_" (cdr x))))
     (,(cut func? <> 'label) .   ; append tags to labels
       ,(lambda (x)
          (set! equation-nr (+ 1 equation-nr))
          (with label-name (number->string equation-nr)
            (ahash-set! labels (cadr x) label-name)
            (list '!concat x `(tag ,label-name))))))))

(define (md-math t)
 "Takes a tree @t, and returns a valid MathJax-compatible LaTeX string"
 (with ltx (math->latex t)
   (serialize-latex (md-math* ltx))))

(define (md-eqref x)
  (let* ((label (cadr x))
         (err-msg (string-append "undefined label " label))
         (label-name (ahash-ref labels label err-msg)))
    (string-append "(" label-name ")")))

(define (md-item? x)
  (and (list>0? x) (func? x 'concat) (== (cadr x) '(item))))

(define (md-list x)
  (let* ((c (cond ((== (car x) 'itemize) "* ")
                  ((== (car x) 'enumerate) "1. ")
                  ((== (car x) 'enumerate-alpha) "a. ")
                  (else "* ")))
         (cs (string-concatenate (make-list (string-length c) " ")))
         (transform
          (lambda (a)
            (if (md-item? a) `(concat ,c ,@(cddr a)) a))))
    (with doc (cAr x)
      (with-global num-line-breaks 1
        (with-global indent (indent-increment cs)
          (with-global first-indent (indent-decrement (string-length c))
            (serialize-markdown `(document ,@(map transform (cdr doc))))))))))

(define (md-quotation x)
  (with-global num-line-breaks 1
    (with-global indent (indent-increment "> ")
      (serialize-markdown (cAr x)))))

(define (style-text style)
 (cond ((== style 'strong) "**")
       ((== style 'em) "*")
       ((== style 'tt) "`")
       ((== style 'strike) "~~")
       (else "")))

(define (md-style x)
  (with st (style-text (car x))
    (string-concatenate 
     `(,st ,@(map serialize-markdown (cdr x)) ,st))))

(define (md-cite x)
  (if (not (hugo-extensions?)) ""
      (string-concatenate
       (list-intersperse
        (map (cut string-append "{{< cite " <> " >}}") (cdr x))
        ", "))))

(define (md-cite-detail x)
  (with detail (cAr x)
      (string-append (md-cite (cDr x)) " (" detail ")")))

(define (md-hlink x)
  (with payload (cdr x)
    (string-append "[" (serialize-markdown payload) "]"
                   "(" (cadr payload) ")")))    

(define (md-figure x)
  "Hugo {{< figure >}} shortcode"
  (if (hugo-extensions?)
      (with payload (cdr x)
        (with-global num-line-breaks 0
          (string-concatenate 
           `("{{< figure src=\"" ,(car payload) 
             "\" title=\"" ,@(map serialize-markdown (cdr payload)) "\" >}}"))))
      ""))

(define (md-footnote x)
  ; Input: (footnote (document [stuff here]))
  (set! footnote-nr (+ 1 footnote-nr))
  (postlude-add (cdr x))
  (string-append "[^" (number->string footnote-nr) "]"))

(define (md-doc-title x)
  (set! doc-title (serialize-markdown (cdr x)))
  (if (hugo-extensions?) ""
      ((md-header 1) (cdr x))))

(define (md-block x)
  (with-global num-line-breaks 1
    (with syntax (tm-ref x 0)
      (string-concatenate 
       `("```" ,syntax "\n" ,@(map serialize-markdown (cdr x)) "```\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define serialize-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l)) 
     (list (list 'strong md-style)
           (list 'em md-style)
           (list 'tt md-style)
           (list 'strike md-style)
           (list 'block md-block)
           (list 'document md-document)
           (list 'quotation md-quotation)
           (list 'theorem md-environment)
           (list 'proposition md-environment)
           (list 'corollary md-environment)
           (list 'lemma md-environment)
           (list 'proof md-environment)
           (list 'math md-math)
           (list 'equation md-math)
           (list 'equation* md-math)
           (list 'concat md-concat)
           (list 'itemize md-list)
           (list 'enumerate md-list)
           (list 'enumerate-alpha md-list)
           (list 'h1 (md-header 1))
           (list 'h2 (md-header 2))
           (list 'h3 (md-header 3))
           (list 'h4 (md-header 4))
           (list 'doc-title md-doc-title)
           (list 'author-name author-add)
           (list 'cite md-cite)
           (list 'cite-detail md-cite-detail)
           (list 'eqref md-eqref)
           (list 'footnote md-footnote)
           (list 'figure md-figure)
           (list 'hlink md-hlink)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (serialize-markdown x)
  (cond ((null? x) "")
        ((string? x) (md-string x))
        ((symbol? x) 
         (display* "Ignoring symbol " x "\n")
         "")
        ((symbol? (car x))
         (with fun 
              (ahash-ref serialize-hash (car x))
            (if (!= fun #f)
                (fun x)
                (begin
                  (display* "Skipped " (car x) "\n")
                  (skip x)))))
        (else
         (apply string-append 
                (cons (serialize-markdown (car x))
                      (map serialize-markdown (cdr x)))))))

(tm-define (serialize-markdown-document x)
  (with-global labels (make-ahash-table)
    (with-global footnote-nr 0
      (with-global equation-nr 0
        (with-global authors '()
          (with-global postlude ""
            (with body (serialize-markdown x)
              (string-append (prelude)
                             body
                             postlude))))))))
  
