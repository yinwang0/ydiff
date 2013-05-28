#lang racket

(require "structs.rkt")
(require "parse-scheme.rkt")
(require "diff.rkt")
(require "utils.rkt")


(define *keywords*
  '(define defun defvar lambda cond if else
     let let* let-values let*-values
     while for define-syntax syntax-rules
     define-minor-mode))

(define *defs*
  '(define defun defvar define-syntax define-minor-mode))


(define get-keyword
  (lambda (node)
    (match node
      [(Node type _ _ elts)
       (cond
        [(not (pair? elts)) #f]
        [(null? elts) #f]
        [else
         (let ([sym (get-symbol (car elts))])
           (cond
            [(memq sym *keywords*) sym]
            [else #f]))])]
      [_ #f])))


;; Try to get the keyword of the sexp if it is not token, comment, str
;; and char.
(set-get-type
  (lambda (node)
    (cond
     [(token? node) 'token]
     [(comment? node) 'comment]
     [(str? node) 'str]
     [(character? node) 'char]
     [else
      (get-keyword node)])))


(set-get-name
  (lambda (node)
    (let ([key (get-keyword node)])
      (cond
       [(and key
             (memq key *defs*)
             (pair? (Node-elts node))
             (not (null? (cdr (Node-elts node)))))
        (get-symbol (cadr (Node-elts node)))]
       [else #f]))))


;; function interface
(define diff-scheme
  (lambda (file1 file2)
    (let* ([s1 (read-file file1)]
           [s2 (read-file file2)]
           [node1 (parse-scheme s1)]
           [node2 (parse-scheme s2)])
      (diff node1 node2 file1 file2))))

;; (diff-scheme "demos/mk1.ss" "demos/mk2.ss")


;; command line interface
(let* ([args (current-command-line-arguments)]
       [file1 (vector-ref args 0)]
       [file2 (vector-ref args 1)])
  (diff-scheme file1 file2))
