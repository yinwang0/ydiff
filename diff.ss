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



(load "utils.ss")



;-------------------------------------------------------------
;                      parameters
;-------------------------------------------------------------

;; The ratio of cost/size that we consider two nodes to be
;; "similar", so as to perform a heuristic move (that will
;; cut running time by a lot.) But this number should be
;; small enough otherwise everything will be considered to
;; be moves! Set to a small number for accuracy.
(define *move-ratio* 0)


;; The minimum size of a node to be considered for moves.
;; Shouldn't be too small, otherwise small deletec names
;; will appear in a very distant place!
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

(struct Change (orig cur cost type) #:transparent)
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


;; create a "total change". (delete node1 and insert node2)
(define total
  (lambda (node1 node2)
    (let ([size1 (node-size node1)]
          [size2 (node-size node2)])
      (values (append (del node1) (ins node2))
              (+ size1 size2)))))


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
      [(Expr 'frame elts _ _)
       (apply append (map deframe elts))]
     [else (list node)])))


(define deframe-change
  (lambda (change)
    (cond
     [(ins? change)
      (apply append
             (map ins (deframe (Change-cur change))))]
     [(del? change)
      (apply append
             (map del (deframe (Change-orig change))))]
     [else (list change)])))


(define extract-frame
  (lambda (node1 node2 type)
    (match node1
      [(Expr type1 elts1 start1 end1)
       (let ([frame-elts (filter (lambda (x)
                                   (not (eq? x node2)))
                                 elts1)])
         (type (Expr 'frame frame-elts start1 start1)))]
      [_ fatal 'extract-frame "I only accept Expr"])))


;; (define n1 (Token "ok" 0 1))
;; (define n2 (Expr 'ok (list n1 (Token "bar" 1 2)) 0 2))
;; (map deframe-change (extract-frame n2 n1 ins))






;------------------ operations on nodes ---------------------

;; "virtual function" - get definition name
;; should be overridden by different languages
(define get-name (lambda (node) #f))


;; "virtual function" - get node type
(define get-type
  (lambda (node)
    (cond
     [(Expr? node) (Expr-type node)]
     [(Token? node) 'token]
     [(Comment? node) 'comment]
     [(Str? node) 'str]
     [(Char? node) 'char])))


(define same-def?
  (lambda (e1 e2)
    (cond
     [(not (eq? (get-type e1) (get-type e2)))
      #f]
     [else
      (let ([name1 (get-name e1)]
            [name2 (get-name e2)])
        (and name1 name2 (equal? name1 name2)))])))


(define different-def?
  (lambda (e1 e2)
    (cond
     [(not (eq? (get-type e1) (get-type e2)))
      #f]
     [else
      (let ([name1 (get-name e1)]
            [name2 (get-name e2)])
        (and name1 name2 (not (equal? name1 name2))))])))




;; whether two nodes are similar given the cost
(define similar?
  (lambda (node1 node2 c)
    (<= c (* *move-ratio* (+ (node-size node1)
                             (node-size node2))))))



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
     [(or (Token? node) (Str? node) (Char? node)) 1]
     [(Expr? node)
      (cond
       [(hash-has-key? *node-size-hash* node)
        (hash-ref *node-size-hash* node)]
       [else
        (memo (node-size (Expr-elts node)))])]
     [else 0])))


(define node-depth
  (lambda (node)
    (cond
     [(null? node) 0]
     [(pair? node)
      (apply max (map node-depth node))]
     [(Expr? node)
      (add1 (node-depth (Expr-elts node)))]
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
    (let ([total (+ (node-size (Change-orig change))
                    (node-size (Change-cur change)))])
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
              (similar? node1 node2 cost))
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
     [(and (Char? node1) (Char? node2))
      (diff-string (char->string (Char-c node1))
                   (char->string (Char-c node2))
                   node1 node2)]
     [(and (Str? node1) (Str? node2))
      (diff-string (Str-text node1) (Str-text node2) node1 node2)]
     [(and (Comment? node1) (Comment? node2))
      (diff-string (Comment-text node1) (Comment-text node2) node1 node2)]
     [(and (Token? node1) (Token? node2))
      (diff-string (Token-text node1) (Token-text node2) node1 node2)]
     [(and (Expr? node1) (Expr? node2)
           (eq? (get-type node1) (get-type node2)))
      (letv ([(m c) (diff-list (Expr-elts node1) (Expr-elts node2) move?)])
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





;; global 2D hash for storing known diffs
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
                (and (not (different-def? (car ls1) (car ls2)))
                     (similar? (car ls1) (car ls2) c0)))
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
     [(and (Expr? node1) (Expr? node2))
      (cond
       [(<= (node-size node1) (node-size node2))
        (let loop ([elts2 (Expr-elts node2)])
          (cond
           [(null? elts2) (values #f #f)]
           [else
            (letv ([(m0 c0) (diff-node node1 (car elts2)  move?)])
              (cond
               [(or (same-def? node1 (car elts2))
                    (similar? node1 (car elts2) c0))
                (let ([frame (extract-frame node2 (car elts2) ins)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts2))]))]))]
       [else
        (let loop ([elts1 (Expr-elts node1)])
          (cond
           [(null? elts1) (values #f #f)]
           [else
            (letv ([(m0 c0) (diff-node (car elts1) node2 move?)])
              (cond
               [(or (same-def? (car elts1) node2)
                    (similar? (car elts1) node2 c0))
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
      (big-node? (Change-cur c))]
     [(del? c)
      (big-node? (Change-orig c))]
     [(mod? c)
      (or (big-node? (Change-orig c))
          (big-node? (Change-cur c)))])))


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
        (< (get-start x) (get-start y))]))))



;; iterate diff-list on the list of changes
(define closure
  (lambda (changes)
    (set! *diff-hash* (make-hasheq))
    (let loop ([changes changes] [closed '()] [count 1])
      (printf "~n[closure loop #~a] " count)
      (letv ([dels (filter (predand del? big-change?) changes)]
             [adds (filter (predand ins? big-change?) changes)]
             [rest (set- changes (append dels adds))]
             [ls1 (sort (map Change-orig dels) node-sort-fn)]
             [ls2 (sort (map Change-cur adds) node-sort-fn)]
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
                       (Change-orig (car cs))
                       (Change-cur (car cs)))])
          (cond
           [(or (not key)
                (= (get-start key) (get-end key)))
            (loop (cdr cs) tags)]
           [(and (Change-orig (car cs)) (Change-cur (car cs)))
            (let ([startTag (Tag (link-start (car cs) side)
                                 (get-start key) -1)]
                  [endTag (Tag "</a>" (get-end key) (get-start key))])
              (loop (cdr cs) (cons endTag (cons startTag tags))))]
           [else
            (let ([startTag (Tag (span-start (car cs) side)
                                 (get-start key) -1)]
                  [endTag (Tag "</span>" (get-end key) (get-start key))])
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
                  (Change-orig change)
                  (Change-cur change))]
          [other (if (eq? side 'left)
                     (Change-cur change)
                     (Change-orig change))])
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
      (line port "<LINK href=\"diff-s.css\""
                      " rel=\"stylesheet\" type=\"text/css\">")
      (line port "<script type=\"text/javascript\""
                        " src=\"nav-div.js\"></script>")
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



;; main command
;; takes two file names and a parser
(define diff
  (lambda (file1 file2 parse)
    (cleanup)
    (letv ([start (current-seconds)]
           [s1 (read-file file1)]
           [s2 (read-file file2)]
           [node1 (parse s1)]
           [node2 (parse s2)]
           [_ (diff-progress "\nDone parsing")]
           [(changes cost) (diff-node node1 node2 #f)]
           [_ (diff-progress "\nDone diffing")]
           [changes (closure changes)]
           [_ (diff-progress "\nDone moving")]
           [_ (set! *diff-hash* (make-hasheq))]
           [ctags1 (change-tags changes 'left)]
           [ctags2 (change-tags changes 'right)]
           [tagged1 (apply-tags s1 ctags1)]
           [tagged2 (apply-tags s2 ctags2)]
           [frame-file (string-append (base-name file1) "-"
                                      (base-name file2) ".html")]
           [port (open-output-file frame-file
                                   #:mode 'text
                                   #:exists 'replace)]
           [end (current-seconds)])
      (printf "finished in ~a seconds~n" (- end start))

      (html-header port)
      (write-html port tagged1 "left")
      (write-html port tagged2 "right")
      (html-footer port)
      (close-output-port port)
      (cleanup))))

