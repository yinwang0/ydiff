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



(load "parse-js.ss")
(load "diff.ss")



;-------------------------------------------------------------
;                         overrides
;-------------------------------------------------------------

(define get-name
  (lambda (node)
    (let ([id-exp (match-tags node '(name identifier id))])
      (and id-exp (get-symbol (car (Expr-elts id-exp)))))))


;; (same-def? (car (parse1 $statement "function f(x) {}"))
;;            (car (parse1 $statement "function f(x) {}")))


;; (different-def? (car (parse1 $statement "function f(x) {}"))
;;                 (car (parse1 $statement "function g(x) {}")))


;---------------------------------------------
(define diff-js
  (lambda (file1 file2)
    (load "diff-js.ss")
    (diff file1 file2 parse-js)))

; (diff-js "tests/nav.js" "tests/nav-div.js")

