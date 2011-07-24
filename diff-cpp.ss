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
;                       settings
;-------------------------------------------------------------

(define *move-ratio* 0)
(define *move-size* 5)



;-------------------------------------------------------------
;                      overrides
;-------------------------------------------------------------

(define get-name
  (lambda (node)
    (let ([id-exp
           (get-property
            (get-property
             (get-property node 'name)
             'identifier)
            'id)])
      (and id-exp (get-symbol (car (Expr-elts id-exp)))))))

; (get-name (car (parse1 $statement "int f(int x) {}")))

;; (same-def? (car (parse1 $statement "int f(int x) {}"))
;;            (car (parse1 $statement "int f(int x) {}")))


;; (different-def? (car (parse1 $statement "int f(int x) {}"))
;;                 (car (parse1 $statement "int g(int x) {}")))



;---------------------------------------------
(define diff-cpp
  (lambda (file1 file2)
    (diff file1 file2 parse-cpp)))



;---------------------------------------------
; (diff-cpp "tests/simulator-mips.cc" "tests/simulator-arm.cc")
; (diff-cpp "tests/d8-3404.cc" "tests/d8-8424.cc")

