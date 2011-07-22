;; yDiff - a language-aware tool for comparing programs
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




(load "parse-cpp.ss")
(load "diff.ss")


;-------------------------------------------------------------
;                       diff settings
;-------------------------------------------------------------

(define *move-ratio* 0)
(define *move-size* 7)
(define *move-depth* 1000)
(define *move-iteration* 1000)
(define *min-frame-size* 5)
(define *min-frame-depth* 10000)
(define *max-string-len* 200)



;-------------------------------------------------------------
;                 parameters for the scanner
;-------------------------------------------------------------

(define *keywords* '())
(define *keyword-exchange* '())
(define *defs* '())



;-------------------------------------------------------------
;                      redefinitions
;-------------------------------------------------------------

(define get-property
  (lambda (e type)
    (cond
     [(not (Expr? e)) #f]
     [else
      (let ([matches (filter (lambda (x) (and (Expr? x)
                                              (eq? (Expr-type x) type)))
                             (Expr-elts e))])
        (cond
         [(null? matches) #f]
         [else (car matches)]))])))


(define same-def?
  (lambda (e1 e2)
    (cond
      [(and (Expr? e1) (Expr? e2)
            (or (and (eq? (Expr-type e1) 'function)
                     (eq? (Expr-type e2) 'function))
                (and (eq? (Expr-type e1) 'class)
                     (eq? (Expr-type e2) 'class))))
       (let ([name1 (get-property (get-property e1 'signature) 'name)]
             [name2 (get-property (get-property e2 'signature) 'name)])
         (and name1 name2 (node-equal? name1 name2)))]
      [else #f])))


; (same-def? (Token "foo" 0 1) (Token "foo" 0 1))


;; (same-def?
;;  (Expr 'function (list (Expr 'name (list "foo") 0 4)) 0 10)
;;  (Expr 'function (list (Expr 'name (list "foo") 0 4)) 0 10))

;; (same-def?
;;  (car (parse1 $statement
;;               (read-file "t1.cc")
;;    ))
;;  (car (parse1 $statement
;;               (read-file "t2.cc")
;;    )))



;; (node-equal?
;;  (car (parse1 $expression "LineEditor::LineEditor"))
;;  (car (parse1 $expression "LineEditor::LineEditor")))


; (node-type (car (parse1 $expression "LineEditor::LineEditor")))


;; use name similarity to determine whether they are
;; differnet definitions
(define different-def?
  (lambda (e1 e2)
    (cond
      [(and (Expr? e1) (Expr? e2)
            (or (and (eq? (Expr-type e1) 'function)
                     (eq? (Expr-type e2) 'function))
                (and (eq? (Expr-type e1) 'class)
                     (eq? (Expr-type e2) 'class))))
       (let ([name1 (get-property (get-property e1 'signature) 'name)]
             [name2 (get-property (get-property e2 'signature) 'name)])
         (cond
          [(and name1 name2)
           (not (equal? name1 name2))
           (letv ([(m c) (diff-node name1 name2 0 #t)])
             (> c (* 0 (+ (node-size name1)
                            (node-size name2)))))]
          [else #f]))]
      [else #f])))



;; (define language-specific-include?
;;   (lambda (e)
;;     (and (Expr? e)
;;          (memq (Expr-type e)
;;                '(
;;                  signature
;;                  name
;;                  macro-definition
;;                  if-statement
;;                  switch-statement
;;                  do-while-statement
;;                  while-statement
;;                  for-statement
;;                  for-in-statement
;;                  labelled-statement
;;                  try-statement
;;                  namespace-definition
;;                  using-namespace
;;                  class-definition
;;                  function-definition
;;                  parameters
;;                  initializer
;;                  function-declaration
;;                  variable-definition
;;                  enum-declaration
;;                  expression-statement
;;                  extended-assembly
;;                  inline-assembly
;;                  )))))



;; (define language-specific-similar?
;;   (lambda (e1 e2 c)
;;     (let* ([size1 (node-size e1)]
;;            [size2 (node-size e2)]
;;            [total (+ size1 size2)])
;;       (cond
;;        [(and (Expr? e1) (Expr? e2)
;;              (eq? 'name (Expr-type e1))
;;              (eq? 'name (Expr-type e2)))
;;         (= c 0)]
;;        [else #f]))))




;---------------------------------------------
(define diff-cpp
  (lambda (file1 file2)
    (diff file1 file2 parse-cpp)))



;---------------------------------------------
; (diff-cpp "simulator-mips.cc" "simulator-arm.cc")
; (diff-cpp "tests/d8-3404.cc" "tests/d8-8424.cc")

