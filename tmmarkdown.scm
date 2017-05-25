;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs-stree to markdown-stree converter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown tmmarkdown)
  (:use (convert markdown markdownout)
        ;(convert markdown markdowntm)
        ))

(define (keep x)
  (cons (car x) (map texmacs->markdown (cdr x))))

(define (change-to func)
  (lambda (x)
    (cons func (map texmacs->markdown (cdr x)))))

(define (skip x)
  "Process only the last element. Useful for with tags."
  (texmacs->markdown (cAr x)))

(define (skip-fully x)
  '())

(define (add-doc-data x)
  (skip x))

;TODO: session, code blocks, hlink, href, bibliograpy
(define conversion-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! conversion-hash <> <>) l)) 
     (list (list 'strong keep)
           (list 'dfn (change-to 'strong))
           (list 'em keep)
           (list 'samp (change-to 'tt))
           (list 'python (change-to 'tt))
           (list 'cpp (change-to 'tt))
           (list 'scheme (change-to 'tt))
           (list 'verbatim (change-to 'tt))
           (list 'doc-data add-doc-data)
           (list 'document keep)
           (list 'quotation keep)
           (list 'theorem keep)
           (list 'proposition keep)
           (list 'corollary keep)
           (list 'lemma keep)
           (list 'proof keep)
           (list 'math identity)
           (list 'equation identity)
           (list 'equation* identity)
           (list 'concat keep)
           (list 'doc-title (change-to 'h1))
           (list 'section (change-to 'h2))
           (list 'subsection (change-to 'h3))
           (list 'subsubsection (change-to 'h4))
           (list 'paragraph (change-to 'strong))
           (list 'subparagraph (change-to 'strong))
           (list 'with skip)
           (list 'itemize keep)
           (list 'itemize-minus (change-to 'itemize))
           (list 'itemize-dot (change-to 'itemize))
           (list 'itemize-arrow (change-to 'itemize))
           (list 'enumerate keep)
           (list 'enumerate-roman (change-to 'enumerate))
           (list 'enumerate-Roman (change-to 'enumerate))
           (list 'enumerate-alpha keep)
           (list 'enumerate-Alpha (change-to 'enumerate-alpha))
           (list 'item keep)
           (list 'cite keep)
           (list 'cite-detail keep)
           (list 'hlink keep)
           (list 'bibliography skip-fully)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->markdown x)
  (cond ((not (list>0? x)) x)
        ((symbol? (car x))
         (with fun 
              (ahash-ref conversion-hash (car x))
            (if (!= fun #f)
                (fun x)
                (begin
                  (display* "Skipped " (car x) "\n")
                  (skip x)))))
        (else
         (cons (texmacs->markdown (car x)) (texmacs->markdown (cdr x))))))