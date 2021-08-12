;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markdown-utils.scm
;; DESCRIPTION : misc stuff for the md converter
;; COPYRIGHT   : (C) 2021 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (markdown-utils))

(use-modules (ice-9 regex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state for document serialization and config options
;; Usage is wrapped within a "with-global" in serialize-markdown-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public md-globals (make-ahash-table))

(define-public (md-get what)
  (ahash-ref md-globals what))

(define-public (md-set what value)
  (ahash-set! md-globals what value))

(define-public-macro (with-md-globals var val . body)
  (let ((old (gensym)) (new (gensym)))
    `(let ((,old (md-get ,var)))
       (md-set ,var ,val)
       (let ((,new (begin ,@body)))
         (md-set ,var ,old)
         ,new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for stree transformations
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

(define-public (replace-fun-list where rules)
  (if (and (list>0? rules) (pair? (car rules)))
      (replace-fun (replace-fun-list where (cdr rules))
                   (caar rules) (cdar rules))
      where))

(define-public (stree-contains? st l)
  (or (tm-in? st l)
      (nnull? (select st `(:* (:or ,@l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for string transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-encoding->md-encoding x file?)
  (if file? (string-convert x "Cork" "UTF-8") x))

; "Copy to markdown" already performs some conversion, so we check whether we
; are exporting to file
(define-public (md-encoding->tm-encoding x file?)
  (if file? (string-convert x "UTF-8" "Cork") x))

(define-public (sanitize-selector s)
  "Makes @s safe(r) for use in querySelector(). No guarantees"
  (if (string? s)
      (string-map 
        (lambda (c) (if (char-set-contains? char-set:letter+digit c) c #\-)) s)
      (begin (display* "Labels must be strings. Received: " s "\n") "")))

(define-public (string-punctuation? s)
  (not (string-match "\\w+" s)))

(define-public (string-recompose-space s)
  (string-recompose s " "))

(define-public (string-recompose-newline s)
  (string-recompose s "\n"))

(define-public (string-nnull? s)
  (and (string? s)
       (not (string-null? s))))

(define-public (md-string s)
  ;HACK: tm-encoding (Cork) does not have newlines, so we work around those
  (string-recompose-newline
   (map (cut tm-encoding->md-encoding <> (md-get 'file?))
        (string-split s #\newline))))

(define md-no-first-chars (string->char-set ">-*123456789:"))

(define (maybe-join match prev)
  (with m (match:substring match 0)
    (if (and (list>0? prev)
             (> (string-length m) 0)
             (string-every md-no-first-chars m 0 1))
        (cons (string-append (car prev) " " m) (cdr prev))
        (cons m prev))))

(define (safe-split s)
  "Splits words in @s except when a new word would start with a special char"
  ; HACK: srfi chokes on cork chars, e.g. the backquote \x00 is interpreted
  ; as #\nil, so we need to convert back and forth
  (with s* (tm-encoding->md-encoding s #t)
    (map (cut md-encoding->tm-encoding <> #t)
         (reverse (fold-matches "[^ ]*" s* '() maybe-join)))))

(define (adjust-width* s* cols prefix first-prefix)
  (if (not cols)  ; set width to #f to disable adjustment
      (string-append prefix s*)
      (let* ((s (string-append first-prefix s*))
             (l (safe-split s))
             (c (string-length prefix))
             (line-len 0)
             (proc (lambda (w acc)
                     (set! line-len (+ line-len (string-length w) 1))
                     (if (> line-len cols)
                         (begin
                           (set! line-len (+ c (string-length w) 1))
                           (string-append acc "\n" prefix w " "))
                         (string-append acc w " ")))))
        (string-trim-right (list-fold proc "" l)))))

(define-public (adjust-width s cols prefix first-prefix)
  "Adjusts @s to have @cols width with given prefixes"
  (string-recompose-newline
   (map (cut adjust-width* <> cols prefix first-prefix)
        (string-split s #\newline))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for YAML output
;; Sample stree: (dict "key1" "val1" "key2" (tuple "one" "two" "three"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string->yaml s cols indent)
  "Outputs @s either quoted or justified to @cols if it's an integer"
  (if (and (number? cols) (> (string-length s) (- cols (string-length indent) 2)))
      (string-append ">\n" (adjust-width s cols indent indent))
      (string-append "\"" s "\"")))
  
(define (indent-once indent)
  (string-append indent "  "))

(define (list->yaml l indent)
  (with item->yaml
    (lambda (it) (string-append indent "- "
                   (serialize-yaml it (indent-once indent))))
    (string-append "\n"
      (string-recompose-newline (map item->yaml (cdr l))))))

(define keys<=? 
  (lambda (a b) (string<=? (car a) (car b))))

(define (dict->yaml l indent)
  (with item->yaml 
    (lambda (kv)
      (string-append indent (car kv) ": " 
        (serialize-yaml (cdr kv) (indent-once indent))))
    (string-append "\n"
      (string-recompose-newline 
       (map item->yaml (list-sort (list->assoc (cdr l)) keys<=?))))))

(define (bool-value? x)
  (in? x '("false" "true" "False" "True")))

(define-public (serialize-yaml x . indent*)
  (with indent (if (null? indent*) "" (car indent*))
    (cond ((null? x) "")
          ((bool-value? x) x)
          ((string? x)
           (string->yaml x (md-get 'paragraph-width) (indent-once indent)))
          ((func? x 'pdf-name) (download-name))
          ((func? x 'dict) (dict->yaml x indent))
          ((func? x 'tuple) (list->yaml x indent))
          ((func? x 'date) (second x))
          (else (force-string x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define marginal-styles-table
  (list->ahash-table '(("b" . "bottom") ("c" . "center")
                       ("t" . "top") ("normal" . "right"))))

(define-public (md-marginal-style s)
  (ahash-ref marginal-styles-table s))

(define-public (md-map fun x)
  (list-filter (map fun x) nnull?))

(tm-define (download-name)
  (:secure #t)
  (string-append (url-basename (current-buffer)) ".pdf"))

(define (autoexport-on?)
  (in? (get-preference "texmacs->markdown:auto-export")
       '("relative" "absolute")))

(tm-define (save-buffer . l)
  (:require (autoexport-on?))
  (apply save-buffer-main l)
  (and-with s (get-init-env "markdown-auto-export")
    (with u (string->url s)
      (if (url-rooted? u)
          ((buffer-exporter "markdown") u)
          ((buffer-exporter "markdown")
           (url-append (url-head (current-buffer)) u))))))
