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



(load "parsec.ss")


;-------------------------------------------------------------
;                     scanner setttings
;-------------------------------------------------------------

; single quote is considered a delimeter in s-expression
(define *delims* (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  "," ))

(define *line-comment* (list ";"))
(define *comment-start* "")             ; no block comments for lisp
(define *comment-end* "")
(define *operators*  '())
(define *quotation-marks* '(#\"))
(define *significant-whitespaces* '())



;-------------------------------------------------------------
;                        parser
;-------------------------------------------------------------

(:: $open
     (@or (@~ "(") (@~ "[")))

(:: $close
     (@or (@~ ")") (@~ "]")))

(:: $non-parens
     (@and (@! $open) (@! $close)))

(::= $parens 'sexp
     (@seq $open (@* $sexp) $close))

(:: $sexp
    (@+ (@or $parens $non-parens)))

(:: $program $sexp)


(define parse-scheme
  (lambda (s)
    (first-val ($eval $sexp (scan s)))))

