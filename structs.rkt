#lang racket

(provide (all-defined-out))


;-------------------------------------------------------------
;                       data types
;-------------------------------------------------------------
(struct Node (type start end elts [size #:mutable]) #:transparent)


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


(define decode-ast
  (lambda (exp)
    (match exp
      [`(Node ',type ,start ,end ,elts ,size)
       (Node start end type (decode-ast elts) size)]
      [`(list ,elts ...)
       (map decode-ast elts)]
      [''() '()])))


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


;; Find the first node containing a given path of tags.
;; For example: '(function parameter) could match a function's parameter

(define match-tags
  (lambda (e tags)
    (cond
     [(not (Node? e)) #f]
     [(null? tags) e]
     [else
      (match-tags (get-tag e (car tags)) (cdr tags))])))
