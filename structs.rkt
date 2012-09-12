#lang racket

(provide (all-defined-out))


;-------------------------------------------------------------
;                       data types
;-------------------------------------------------------------
(struct Node    (start end)        #:transparent)
(struct Expr     Node (type elts)  #:transparent)
(struct Token    Node (text)       #:transparent)
(struct Comment  Node (text)       #:transparent)
(struct Str      Node (text)       #:transparent)
(struct Char     Node (text)       #:transparent)
(struct Newline  Node ()           #:transparent)
(struct Phantom  Node ()           #:transparent)



(define decode-ast
  (lambda (exp)
    (match exp
      [`(Expr ,start ,end ',type ,elts)
       (Expr start end type (decode-ast elts))]
      [`(Token ,start ,end ,text)
       (Token start end text)]
      [`(Comment ,start ,end ,text)
       (Comment start end text)]
      [`(Str ,start ,end ,text)
       (Str start end text)]
      [`(Newline ,start ,end)
       (Newline start end)]
      [`(Phantom ,start ,end)
       (Phantom start end)]
      [`(list ,elts ...)
       (map decode-ast elts)]
      [''() '()])))



(define node-type
  (lambda (node)
    (and (Expr? node) (Expr-type node))))


(define get-start
  (lambda (node)
    (Node-start node)))


(define get-end
  (lambda (node)
    (Node-end node)))


(define get-symbol
  (lambda (node)
    (cond
     [(Token? node)
      (string->symbol (Token-text node))]
     [else #f])))


(define get-tag
  (lambda (e tag)
    (let ([matches (filter (lambda (x)
                             (and (Expr? x)
                                  (eq? (Expr-type x) tag)))
                           (Expr-elts e))])
      (cond
       [(null? matches) #f]
       [else (car matches)]))))


(define match-tags
  (lambda (e tags)
    (cond
     [(not (Expr? e)) #f]
     [(null? tags) e]
     [else
      (match-tags (get-tag e (car tags)) (cdr tags))])))

