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



;-------------------------------------------------------------
;                 parameters for the scanner
;-------------------------------------------------------------

(define *keywords* '())
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




;---------------------------------------------
(define diff-cpp
  (lambda (file1 file2)
    (diff file1 file2 parse-cpp)))



;---------------------------------------------
; (diff-cpp "tests/simulator-mips.cc" "tests/simulator-arm.cc")
; (diff-cpp "tests/d8-3404.cc" "tests/d8-8424.cc")

