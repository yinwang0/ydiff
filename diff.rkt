;; ydiff - a language-aware tool for comparing programs
;; Copyright (C) 2011 Yin Wang (yinwang0@gmail.com)


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
;                      parameters
;-------------------------------------------------------------

;; The minimum size of a node to be considered as moved. Shouldn't be
;; too small, otherwise small deleted names may appear in a very
;; distant place!
(define *move-size* 5)


;; How long must a string be in order for us to use string-dist
;; function, which is costly when used on long strings but the most
;; accurate method to use. Currently this parameter is set to 0,
;; effective disables all LCS string comparison. This improves
;; performance while not sacrificing accuracy because the algorithm is
;; AST based.
(define *max-string-len* 0)


;; Only memoize the diff of nodes of size larger than this number.
;; This effectively reduces memory usage.
(define *memo-node-size* 2)





;-------------------------------------------------------------
;                      data types
;-------------------------------------------------------------

;; Change - a change in the data structure
;; - old  : the old version, #f for insertions
;; - new  : the new version, #f for deletions
;; - cost : the cost of change from old to new
;; - type : insertion, deletion, or modification?
(struct Change (old new cost type) #:transparent)

;; HTML tag structure used HTML generation code
(struct Tag (tag idx start) #:transparent)

(define ins?
  (lambda (c)
    (eq? 'ins (Change-type c))))

(define del?
  (lambda (c)
    (eq? 'del (Change-type c))))

(define mod?
  (lambda (c)
    (eq? 'mod (Change-type c))))



;----------------- utils for creating changes ----------------
(define ins
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change #f node size 'ins)))))


(define del
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change node #f size 'del)))))


(define mod
  (lambda (node1 node2 cost)
    (list (Change node1 node2 cost 'mod))))


(define mov
  (lambda (node1 node2 cost)
    (list (Change node1 node2 cost 'mov))))


;; create a "total change"
;; (delete node1 completely and then insert node2)
(define total
  (lambda (node1 node2)
    (let ([size1 (node-size node1)]
          [size2 (node-size node2)])
      (values (append (del node1) (ins node2))
              (+ size1 size2)))))


;; temporary workaround before the algorithm stablizes
(define mod->mov
  (lambda (c)
    (match c
     [(Change node1 node2 cost 'mod)
      (Change node1 node2 cost 'mov)]
     [other other])))



;;------------------ frames utils --------------------
(define deframe
  (lambda (node)
    (match node
      [(Node 'frame _ _ elts)
       (apply append (map deframe elts))]
     [else (list node)])))


(define deframe-change
  (lambda (change)
    (cond
     [(ins? change)
      (apply append
             (map ins (deframe (Change-new change))))]
     [(del? change)
      (apply append
             (map del (deframe (Change-old change))))]
     [else (list change)])))


(define extract-frame
  (lambda (node1 node2 type)
    (match node1
      [(Node type1 start1 end1 elts1)
       (let ([frame-elts (filter (lambda (x)
                                   (not (eq? x node2)))
                                 elts1)])
         (type (Node 'frame start1 start1 frame-elts)))]
      [_ fatal 'extract-frame "I only accept Node"])))


;; (define n1 (Token "ok" 0 1))
;; (define n2 (Expr 'ok 0 2 (list n1 (Token "bar" 1 2))))
;; (map deframe-change (extract-frame n2 n1 ins))






;------------------ operations on nodes ---------------------

;; "virtual function" - get definition name
;; can be overridden by individual languages
(define get-name (lambda (node) #f))

(define set-get-name
  (lambda (fun)
    (set! get-name fun)))


;; "virtual function" - get node type
;; can be overridden by individual languages
(define get-type Node-type)

(define set-get-type
  (lambda (fun)
    (set! get-type fun)))


;; same-def? only depend on get-name, so they need not be overridden
;; by individual languages.
(define same-def?
  (lambda (e1 e2)
    (cond
     [(not (eq? (get-type e1) (get-type e2)))
      #f]
     [else
      (let ([name1 (get-name e1)]
            [name2 (get-name e2)])
        (and name1 name2 (equal? name1 name2)))])))


(define set-same-def
  (lambda (fun)
    (set! same-def? fun)))



;----------- node size function ------------
(define *node-size-hash* (make-hasheq))

(define node-size
  (lambda (node)
    (define memo
      (lambda (v)
        (if (> v 1)
            (hash-set! *node-size-hash* node v)
            (void))
        v))
    (cond
     [(pair? node)
      (apply + (map node-size node))]
     [(or (token? node) (str? node) (character? node)) 1]
     [(Node? node)
      (cond
       [(hash-has-key? *node-size-hash* node)
        (hash-ref *node-size-hash* node)]
       [else
        (memo (node-size (Node-elts node)))])]
     [else 0])))


(define node-depth
  (lambda (node)
    (cond
     [(null? node) 0]
     [(pair? node)
      (apply max (map node-depth node))]
     [(Node? node)
      (add1 (node-depth (Node-elts node)))]
     [else 0])))


; (node-depth (parse-scheme "(lambda (x (x (y)) (y)) x)"))


(define uid
  (let ([count 1]
        [table (box '())])
    (lambda (node)
      (let ([p (assq node (unbox table))])
        (cond
         [(not p)
          (let ([id count])
            (set! count (add1 count))
            (set-box! table (cons `(,node . ,id) (unbox table)))
            id)]
         [else
          (cdr p)])))))



;; similarity string from a change
(define similarity
  (lambda (change)
    (let ([total (+ (node-size (Change-old change))
                    (node-size (Change-new change)))])
      (cond
       [(or (= 0 total) (= 0 (Change-cost change)))
        "100%"]
       [else
        (string-append
         (real->decimal-string
          (* 100 (- 1.0 (/ (Change-cost change) total))) 1)
         "%")]))))



;-------------------------------------------------------------
;                       diff proper
;-------------------------------------------------------------

; 2-D memoization table
(define make-table
  (lambda (dim1 dim2)
    (let ([vec (make-vector (add1 dim1))])
      (let loop ([n 0])
        (cond
         [(= n (vector-length vec)) vec]
         [else
          (vector-set! vec n (make-vector (add1 dim2) #f))
          (loop (add1 n))])))))


(define table-ref
  (lambda (t x y)
    (let ([row (vector-ref t x)])
      (vector-ref row y))))


(define table-set!
  (lambda (t x y v)
    (let ([row (vector-ref t x)])
      (vector-set! row y v))))



;---------------- string distance function -----------------

;; string distance is no longer used because string=? saffice to
;; compare strings in ASTs. Retain it here for possible later uses.
(define string-dist
  (lambda (s1 s2)
    (let* ([len1 (string-length s1)]
           [len2 (string-length s2)]
           [t (make-table len1 len2)]
           [char-dist (dist1 t s1 0 s2 0)])
      (cond
       [(= 0 (+ len1 len2)) 0]
       [else
        (/ (* 2.0 char-dist) (+ len1 len2))]))))


(define dist1
  (lambda (table s1 start1 s2 start2)
    (define memo
      (lambda (value)
        (table-set! table start1 start2 value)
        value))
    (cond
     [(table-ref table start1 start2)
      => (lambda (cached) cached)]
     [(= start1 (string-length s1))
      (memo (- (string-length s2) start2))]
     [(= start2 (string-length s2))
      (memo (- (string-length s1) start1))]
     [else
      (let* ([c1 (string-ref s1 start1)]
             [c2 (string-ref s2 start2)]
             [d0 (cond
                  [(char=? c1 c2) 0]
                  [(char=? (char-downcase c1)
                           (char-downcase c2)) 1]
                  [else 2])]
             [d1 (+ d0 (dist1 table s1 (add1 start1) s2 (add1 start2)))]
             [d2 (+ 1 (dist1 table s1 (add1 start1) s2 start2))]
             [d3 (+ 1 (dist1 table s1 start1 s2 (add1 start2)))])
        (memo (min d1 d2 d3)))])))





;--------------------- the primary diff function -------------------
(define diff-node
  (lambda (node1 node2 move?)

    (define memo
      (lambda (v1 v2)
        (and (> (node-size node1) *memo-node-size*)
             (> (node-size node2) *memo-node-size*)
             (hash-put! *diff-hash* node1 node2 (cons v1 v2)))
        (values v1 v2)))

    (define try-extract
      (lambda (changes cost)
        (cond
         [(or (not move?)
              (zero? cost))
          (memo changes cost)]
         [else
          (letv ([(m c) (diff-extract node1 node2 move?)])
            (cond
             [(not m)
              (memo changes cost)]
             [else
              (memo m c)]))])))


    (diff-progress 1) ;; progress bar

    (cond
     [(hash-get *diff-hash* node1 node2)
      => (lambda (cached)
           (values (car cached) (cdr cached)))]
     [(and (character? node1) (character? node2))
      (diff-string (char->string (Node-elts node1))
                   (char->string (Node-elts node2))
                   node1 node2)]
     [(and (str? node1) (str? node2))
      (diff-string (Node-elts node1) (Node-elts node2) node1 node2)]
     [(and (comment? node1) (comment? node2))
      (diff-string (Node-elts node1) (Node-elts node2) node1 node2)]
     [(and (token? node1) (token? node2))
      (diff-string (Node-elts node1) (Node-elts node2) node1 node2)]
     [(and (Node? node1) (Node? node2)
           (eq? (get-type node1) (get-type node2)))
      (letv ([(m c) (diff-list (Node-elts node1) (Node-elts node2) move?)])
        (try-extract m c))]
     [(and (pair? node1) (not (pair? node2)))
      (diff-list node1 (list node2) move?)]
     [(and (not (pair? node1)) (pair? node2))
      (diff-list (list node1) node2 move?)]
     [(and (pair? node1) (pair? node2))
      (diff-list node1 node2 move?)]
     [else
      (letv ([(m c) (total node1 node2)])
        (try-extract m c))])))




;; helper for nodes with string contents (Str, Comment, Token etc.)
(define diff-string
  (lambda (string1 string2 node1 node2)
    (cond
     [(or (> (string-length string1) *max-string-len*)
          (> (string-length string2) *max-string-len*))
      (cond
       [(string=? string1 string2)
        (values (mod node1 node2 0) 0)]
       [else
        (total node1 node2)])]
     [else
      (let ([cost (string-dist string1 string2)])
        (values (mod node1 node2 cost) cost))])))





;; global 2-D hash for storing known diffs
(define *diff-hash* (make-hasheq))

(define diff-list
  (lambda (ls1 ls2 move?)
    (let ([ls1 (sort ls1 node-sort-fn)]
          [ls2 (sort ls2 node-sort-fn)])
      (diff-list1 (make-hasheq) ls1 ls2 move?))))


(define diff-list1
  (lambda (table ls1 ls2 move?)

    (define memo
      (lambda (v1 v2)
        (hash-put! table ls1 ls2 (cons v1 v2))
        (values v1 v2)))

    (define guess
      (lambda (ls1  ls2)
        (letv ([(m0 c0) (diff-node (car ls1) (car ls2) move?)]
               [(m1 c1) (diff-list1 table (cdr ls1) (cdr ls2) move?)]
               [cost1 (+ c0 c1)])
          (cond
           [(or (same-def? (car ls1) (car ls2))
                (zero? c0))
            (memo (append m0 m1) cost1)]
           [else
            (letv ([(m2 c2) (diff-list1 table (cdr ls1) ls2  move?)]
                   [(m3 c3) (diff-list1 table ls1 (cdr ls2) move?)]
                   [cost2 (+ c2 (node-size (car ls1)))]
                   [cost3 (+ c3 (node-size (car ls2)))])
              (cond
               [(<= cost2 cost3)
                (memo (append (del (car ls1)) m2) cost2)]
               [else
                (memo (append (ins (car ls2)) m3) cost3)]))]))))

    (cond
     [(hash-get table ls1 ls2)
      => (lambda (cached)
           (values (car cached) (cdr cached)))]
     [(and (null? ls1) (null? ls2))
      (values '() 0)]
     [(null? ls1)
      (let ([changes (apply append (map ins ls2))])
        (values changes (node-size ls2)))]
     [(null? ls2)
      (let ([changes (apply append (map del ls1))])
        (values changes (node-size ls1)))]
     [else
      (guess ls1 ls2)])))




;; structure extraction
(define diff-extract
  (lambda (node1 node2 move?)
    (cond
     [(or (< (node-size node1) *move-size*)
          (< (node-size node2) *move-size*))
      (values #f #f)]
     [(and (Node? node1) (Node? node2))
      (cond
       [(<= (node-size node1) (node-size node2))
        (let loop ([elts2 (Node-elts node2)])
          (cond
           [(null? elts2) (values #f #f)]
           [else
            (letv ([(m0 c0) (diff-node node1 (car elts2)  move?)])
              (cond
               [(or (same-def? node1 (car elts2))
                    (zero? c0))
                (let ([frame (extract-frame node2 (car elts2) ins)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts2))]))]))]
       [else
        (let loop ([elts1 (Node-elts node1)])
          (cond
           [(null? elts1) (values #f #f)]
           [else
            (letv ([(m0 c0) (diff-node (car elts1) node2 move?)])
              (cond
               [(or (same-def? (car elts1) node2)
                    (zero? c0))
                (let ([frame (extract-frame node1 (car elts1) del)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts1))]))]))])]
     [else (values #f #f)])))





;-------------------------------------------------------------
;                    finding moves
;-------------------------------------------------------------

(define big-node?
  (lambda (node)
    (>= (node-size node) *move-size*)))


(define big-change?
  (lambda (c)
    (cond
     [(ins? c)
      (big-node? (Change-new c))]
     [(del? c)
      (big-node? (Change-old c))]
     [(mod? c)
      (or (big-node? (Change-old c))
          (big-node? (Change-new c)))])))


(define node-sort-fn
  (lambda (x y)
    (let ([name1 (get-name x)]
          [name2 (get-name y)])
      (cond
       [(and name1 name2)
        (string<? (symbol->string name1)
                  (symbol->string name2))]
       [(and name1 (not name2)) #t]
       [(and (not name1) name2) #f]
       [else
        (< (Node-start x) (Node-start y))]))))



;; iterate diff-list on the list of changes
(define closure
  (lambda (changes)
    (set! *diff-hash* (make-hasheq))
    (let loop ([changes changes] [closed '()] [count 1])
      (printf "~n[move pass #~a] " count)
      (letv ([dels (filter (predand del? big-change?) changes)]
             [adds (filter (predand ins? big-change?) changes)]
             [rest (set- changes (append dels adds))]
             [ls1 (sort (map Change-old dels) node-sort-fn)]
             [ls2 (sort (map Change-new adds) node-sort-fn)]
             [(m c) (diff-list ls1 ls2 #t)]
             [new-moves (map mod->mov (filter mod? m))])
        (printf "~n~a new moves found" (length new-moves))
        (cond
         [(null? new-moves)
          (let ([all-changes (append m rest closed)])
            (apply append (map deframe-change all-changes)))]
         [else
          (let ([new-changes (filter (negate mod?) m)])
            (loop new-changes
                  (append new-moves rest closed)
                  (add1 count)))])))))





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
     [(and (eq? (Change-type change) 'mov)
           (> (Change-cost change) 0))
      "mc"]                                     ; move-change
     [(eq? (Change-type change) 'mov) "m"]      ; move
     [(> (Change-cost change) 0) "c"]           ; change
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
    (let ([cls (if (eq? 'del (Change-type change)) "d" "i")]) ; del or ins
      (string-append "<span class=" (qs cls) ">"))))



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



;; poor man's progress bar
(define diff-progress
  (new-progress 10000))



(define cleanup
  (lambda ()
    (set! *node-size-hash* (make-hasheq))
    (set! *diff-hash* (make-hasheq))))



;; main diff function
;; returns all changes after diffing and moving
(define diff
  (lambda (node1 node2)
    (letv ([start (current-seconds)]
           [(changes _) (diff-node node1 node2 #f)]
           [_ (diff-progress "\nDone diffing")]
           [changes (closure changes)]
           [_ (diff-progress "\nDone moving")]
           [end (current-seconds)])
      (printf "finished in ~a seconds~n" (- end start))
      (cleanup)
      changes)))



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

