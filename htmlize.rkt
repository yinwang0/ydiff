;; ydiff - a language-aware tool for comparing programs
;; Copyright (C) 2011-2013 Yin Wang (yinwang0@gmail.com)


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require "utils.rkt")
(require "structs.rkt")

(provide (all-defined-out))



;-------------------------------------------------------------
;                      HTML generation
;-------------------------------------------------------------

;----------------- utils ----------------
(define qs
  (lambda (x)
    (let ([x (cond
              [(symbol? x) (symbol->string x)]
              [(number? x) (number->string x)]
              [(string? x) x])])
      (string-append "'" x "'"))))


(define line
  (lambda (port . s)
    (display (string-append (apply string-append s) "\n") port)))



(define change-tags
  (lambda (changes side)
    (let loop ([cs changes] [tags '()])
      (cond
       [(null? cs) tags]
       [else
        (let ([key (if (eq? side 'left)
                       (Change-old (car cs))
                       (Change-new (car cs)))])
          (cond
           [(or (not key)
                (= (Node-start key) (Node-end key)))
            (loop (cdr cs) tags)]
           [(and (Change-old (car cs)) (Change-new (car cs)))
            (let ([startTag (Tag (link-start (car cs) side)
                                 (Node-start key) -1)]
                  [endTag (Tag "</a>" (Node-end key) (Node-start key))])
              (loop (cdr cs) (cons endTag (cons startTag tags))))]
           [else
            (let ([startTag (Tag (span-start (car cs) side)
                                 (Node-start key) -1)]
                  [endTag (Tag "</span>" (Node-end key) (Node-start key))])
              (loop (cdr cs) (cons endTag (cons startTag tags))))]))]))))


(define apply-tags
  (lambda (s tags)
    (let ([tags (sort tags tag-sort-fn)])
      (let loop ([tags tags] [curr 0] [out '()])
        (cond
         [(null? tags)
          (cond
           [(< curr (string-length s))
            (loop tags (add1 curr) (cons (escape (string-ref s curr)) out))]
           [else
            (apply string-append (reverse out))])]
         [else
          (cond
           [(< curr (Tag-idx (car tags)))
            (loop tags (add1 curr) (cons (escape (string-ref s curr)) out))]
           [else
            (loop (cdr tags) curr (cons (Tag-tag (car tags)) out))])])))))



;; get the CSS class for the change
(define change-class
  (lambda (change)
    (cond
     [(mov? change) "m"]                        ; move
     [(del? change) "d"]
     [(ins? change) "i"]
     [else "u"])))                              ; unchanged



(define link-start
  (lambda (change side)
    (let ([cls (change-class change)]
          [me (if (eq? side 'left)
                  (Change-old change)
                  (Change-new change))]
          [other (if (eq? side 'left)
                     (Change-new change)
                     (Change-old change))])
      (string-append
       "<a id="  (qs (uid me))
       " tid="   (qs (uid other))
       " class=" (qs cls)
       ">"))))


(define span-start
  (lambda (change side)
    (string-append "<span class=" (qs (change-class change)) ">")))


(define tag-sort-fn
  (lambda (t1 t2)
    (cond
     [(= (Tag-idx t1) (Tag-idx t2))
      (> (Tag-start t1) (Tag-start t2))]
     [else
      (< (Tag-idx t1) (Tag-idx t2))])))


(define *escape-table*
  '((#\"  .   "&quot;")
    (#\'  .    "&#39;")
    (#\<  .    "&lt;")
    (#\>  .    "&gt;")))


(define escape
  (lambda (c)
    (cond
     [(assq c *escape-table*) => cdr]
     [else (char->string c)])))



; getting the base name of a path/file name
; (base-name "mk/mk-c.scm") => mk-c
(define base-name
  (lambda (fn)
    (let loop ([i (- (string-length fn) 1)]
               [start -1]
               [end (- (string-length fn) 1)])
      (cond
       [(= i 0)
        (substring fn i end)]
       [(eq? (string-ref fn i) #\.)
        (loop (sub1 i) start i)]
       [(eq? (string-ref fn i) #\/)
        (substring fn (add1 i) end)]
       [else
        (loop (sub1 i) start end)]))))



(define html-header
  (lambda (port)
      (line port "<html>")
      (line port "<head>")
      (line port "<META http-equiv=\"Content-Type\""
                      " content=\"text/html; charset=utf-8\">")
      (line port "<LINK href=\"diff.css\""
                      " rel=\"stylesheet\" type=\"text/css\">")
      (line port "<script type=\"text/javascript\""
                        " src=\"nav.js\"></script>")
      (line port "</head>")
      (line port "<body>")))



(define html-footer
  (lambda (port)
    (line port "</body>")
    (line port "</html>")))



(define write-html
  (lambda (port text side)
    (line port (string-append "<div id=\"" side "\" class=\"src\">"))
    (line port "<pre>")
    (if (string=? side "left")
        (line port "<a id='leftstart' tid='rightstart'></a>")
        (line port "<a id='rightstart' tid='leftstart'></a>"))
    (line port text)
    (line port "</pre>")
    (line port "</div>")))


(define htmlize
  (lambda (changes file1 file2 text1 text2)
    (letv ([tags1 (change-tags changes 'left)]
           [tags2 (change-tags changes 'right)]
           [tagged-text1 (apply-tags text1 tags1)]
           [tagged-text2 (apply-tags text2 tags2)]
           [out-file (string-append (base-name file1) "-"
                                    (base-name file2) ".html")]
           [port (open-output-file out-file
                                   #:mode 'text
                                   #:exists 'replace)])
      (html-header port)
      (write-html port tagged-text1 "left")
      (write-html port tagged-text2 "right")
      (html-footer port)
      (close-output-port port))))

