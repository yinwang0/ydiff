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



(load "parse-scheme.ss")
(load "diff.ss")



;-------------------------------------------------------------
;                         overrides
;-------------------------------------------------------------

(define *keywords*
  '(define defun defvar lambda cond if else
     let let* let-values let*-values
     while for define-syntax syntax-rules
     define-minor-mode))

(define *defs*
  '(define defun defvar define-syntax define-minor-mode))


;; helper for get-type
(define get-keyword
  (lambda (node)
    (match node
      [(Expr _ _ type elts)
       (cond
        [(null? elts) #f]
        [else
         (let ([sym (get-symbol (car elts))])
           (cond
            [(memq sym *keywords*) sym]
            [else #f]))])]
      [_ #f])))


; (get-keyword (car (parse-scheme "(defvar f 1)")))


;; We need to override get-type because
;; S-expression-based languages are flexible about their
;; syntax and don't have rigid types attached to their AST
;; nodes.

;; override
(define get-type
  (lambda (node)
    (cond
     [(Expr? node)
      (get-keyword node)]
     [(Token? node) 'token]
     [(Comment? node) 'comment]
     [(Str? node) 'str]
     [(Char? node) 'char])))



;; override
(define get-name
  (lambda (node)
    (let ([key (get-keyword node)])
      (cond
       [(and key (memq key *defs*))
        (get-symbol (cadr (Expr-elts node)))]
       [else #f]))))


;; (same-def? (car (parse-scheme "(define f 1)"))
;;            (car (parse-scheme "(define f 1)")))

;; (different-def? (car (parse-scheme "(define f 1)"))
;;                 (car (parse-scheme "(define g 1)")))



;-----------------------------------------
(define diff-scheme
  (lambda (file1 file2)
    (diff file1 file2 parse-scheme)))



; (diff-scheme "demos/paredit20.el" "demos/paredit22.el")
; (diff-scheme "demos/mk.scm" "demos/mk-c.scm")
