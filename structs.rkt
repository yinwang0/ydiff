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

(provide (all-defined-out))


;-------------------------------------------------------------
;                       data types
;-------------------------------------------------------------


;---------------------------- Node ---------------------------
(struct Node (type 
              start 
              end 
              elts 
              [size #:mutable]
              [ctx #:mutable]) 
        #:transparent)


(define comment?
  (lambda (n)
    (and (Node? n) (eq? 'comment (Node-type n)))))

(define phantom?
  (lambda (n)
    (and (Node? n) (eq? 'phantom (Node-type n)))))

(define token?
  (lambda (n)
    (and (Node? n) (eq? 'token (Node-type n)))))

(define str?
  (lambda (n)
    (and (Node? n) (eq? 'str (Node-type n)))))

(define character?
  (lambda (n)
    (and (Node? n) (eq? 'char (Node-type n)))))

(define newline?
  (lambda (n)
    (and (Node? n) (eq? 'newline (Node-type n)))))


;----------- node size function ------------
(define node-size
  (lambda (node)
    (cond
     [(and (Node? node) (Node-size node))
      (Node-size node)]
     [(pair? node)
      (apply + (map node-size node))]
     [(or (token? node) (str? node) (character? node)) 1]
     [(Node? node)
      (let ([size (node-size (Node-elts node))])
        (set-Node-size! node size)
        size)]
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


(define set-node-context
  (lambda (node ctx)
    (cond
     [(pair? node)
      (map (lambda (n) (set-node-context n ctx)) node)]
     [(Node? node)
      (let ([name (or (get-name node) ctx)])
        (set-Node-ctx! node name)
        (set-node-context (Node-elts node) name))])))


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



;---------------------------- Change ---------------------------
;; Change - a change in the data structure
;; - old  : the old version, #f for insertions
;; - new  : the new version, #f for deletions
;; - cost : the cost of change from old to new
;; - type : insertion, deletion, or modification?
(struct Change (old new cost type) #:transparent)

(define ins?
  (lambda (c)
    (eq? 'ins (Change-type c))))

(define del?
  (lambda (c)
    (eq? 'del (Change-type c))))

(define mov?
  (lambda (c)
    (eq? 'mov (Change-type c))))


;----------------- utils for creating changes ----------------
(define ins
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change #f node size 'ins)))))

(define del
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change node #f size 'del)))))

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



;---------------------------- Tag ---------------------------
;; HTML tag structure used HTML generation code
(struct Tag (tag idx start) #:transparent)


(define get-symbol
  (lambda (node)
    (cond
     [(token? node)
      (string->symbol (Node-elts node))]
     [else #f])))


;; Find the first node elements which matches a given tag.
(define get-tag
  (lambda (node tag)
    (cond
     [(not (Node? node)) #f]
     [(not (pair? (Node-elts node))) #f]
     [(null? (Node-elts node)) #f]
     [else
      (let ([matches (filter (lambda (x)
                               (eq? (Node-type x) tag))
                             (Node-elts node))])
        (cond
         [(null? matches) #f]
         [else (car matches)]))])))


;; (get-tag (car (parse1 $statement "function f(x) {}"))
;;          'name)


;; Find the first node containing a given path of tags.
;; For example: '(function parameter) could match a function's parameter

(define match-tags
  (lambda (e tags)
    (cond
     [(not (Node? e)) #f]
     [(null? tags) e]
     [else
      (match-tags (get-tag e (car tags)) (cdr tags))])))


;; (match-tags (car (parse1 $statement "function f(x) {}"))
;;             '(function name))


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
