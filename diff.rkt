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

(require "structs.rkt")
(require "utils.rkt")


(provide (all-defined-out))


;-------------------------------------------------------------
;                      parameters
;-------------------------------------------------------------

;; The minimum size of a node to be considered as moved. Shouldn't be
;; too small, otherwise small deleted names may appear in a very
;; distant place!
(define *move-size* 5)


;; Similar to *move-size*, but this number is used for internal moves inside a
;; named body (for example a function). This number can be smaller than
;; *move-size*, usually set to 2 for maxmum accuracy without much noise.
(define *inner-move-size* 2)


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




;;------------------ frames utils --------------------
(define deframe
  (lambda (node)
    (match node
      [(Node 'frame _ _ elts _ _)
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
      [(Node type1 start1 end1 elts1 size ctx)
       (let ([frame-elts (filter (lambda (x)
                                   (not (eq? x node2)))
                                 elts1)])
         (type (Node 'frame start1 start1 frame-elts (- size (node-size node2)) ctx)))]
      [_ fatal 'extract-frame "I only accept Node"])))


;; (define n1 (Token "ok" 0 1))
;; (define n2 (Expr 'ok 0 2 (list n1 (Token "bar" 1 2))))
;; (map deframe-change (extract-frame n2 n1 ins))



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



(define same-ctx?
  (lambda (x y)
    (and (Node? x)
         (Node? y)
         (Node-ctx x)
         (Node-ctx y)
         (>= (node-size x) *inner-move-size*)
         (>= (node-size y) *inner-move-size*)
         (eq? (Node-ctx x) (Node-ctx y)))))


;; structure extraction
(define diff-extract
  (lambda (node1 node2 move?)
    (cond
     [(and (Node? node1) (Node? node2)
           (or (same-ctx? node1 node2)
               (and (>= (node-size node1) *move-size*)
                    (>= (node-size node2) *move-size*))))
      (cond
       [(<= (node-size node1) (node-size node2))
        (let loop ([elts2 (Node-elts node2)])
          (cond
           [(pair? elts2)
            (letv ([(m0 c0) (diff-node node1 (car elts2) move?)])
              (cond
               [(or (same-def? node1 (car elts2))
                    (and (zero? c0)
                         (or (> (node-size node1) *move-size*)
                             (same-ctx? node1 (car elts2)))))
                (let ([frame (extract-frame node2 (car elts2) ins)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts2))]))]
           [else
            (values #f #f)]))]
       [else
        (let loop ([elts1 (Node-elts node1)])
          (cond
           [(pair? elts1)
            (letv ([(m0 c0) (diff-node (car elts1) node2 move?)])
              (cond
               [(or (same-def? (car elts1) node2)
                    (and (zero? c0)
                         (or (> (node-size node2) *move-size*)
                             (same-ctx? (car elts1) node2))))
                (let ([frame (extract-frame node1 (car elts1) del)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts1))]))]
           [else
            (values #f #f)]))])]
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
(define find-moves
  (lambda (changes)
    (set! *diff-hash* (make-hasheq))
    (let loop ([changes changes] [closed '()] [count 1])
      (letv ([dels (filter (predand del? big-change?) changes)]
             [adds (filter (predand ins? big-change?) changes)]
             [rest (set- changes (append dels adds))]
             [ls1 (sort (map Change-old dels) node-sort-fn)]
             [ls2 (sort (map Change-new adds) node-sort-fn)]
             [(m c) (diff-list ls1 ls2 #t)]
             [new-moves (map mod->mov (filter mod? m))])
        (cond
         [(null? new-moves)
          (let ([all-changes (append m rest closed)])
            (apply append (map deframe-change all-changes)))]
         [else
          (let ([new-changes (filter (negate mod?) m)])
            (loop new-changes
                  (append new-moves rest closed)
                  (add1 count)))])))))


;; poor man's progress bar
(define diff-progress
  (new-progress 10000))


(define cleanup
  (lambda ()
    (set! *diff-hash* (make-hasheq))))


;; main diff function
;; returns all changes after diffing and moving
(define diff
  (lambda (node1 node2)
    (letv ([start (current-seconds)]    ; start timer
           [size1 (node-size node1)]
           [size2 (node-size node2)])

      (printf "[info] size of program 1: ~a~n" size1)
      (printf "[info] size of program 2: ~a~n" size2)

      (set-node-context node1 'top)
      (set-node-context node2 'top)

      (printf "[diffing]~n")

      (letv ([(changes cost) (diff-node node1 node2 #f)])
        (diff-progress 'reset)
        (printf "~n[moving]~n")
        (letv ([changes (find-moves changes)]
               [end (current-seconds)])
          (printf "~n[finished] ~a seconds~n" (- end start))
          (cleanup)
          changes)))))
