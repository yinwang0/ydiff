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
;                        utilities
;-------------------------------------------------------------

(define-syntax letv
  (syntax-rules ()
    [(_ () body ...)
     (begin body ...)]
    [(_ ([(e1 e2* ...) e3] bd* ...) body ...)
     (let-values ([(e1 e2* ...) e3])
       (letv (bd* ...) body ...))]
    [(_ ([e1 e2] bd* ...) body ...)
     (let ([e1 e2])
       (letv (bd* ...) body ...))]))


(define-syntax first-val
  (syntax-rules ()
    [(_ e)
     (letv ([(x y) e]) x)]))


(define-syntax second-val
  (syntax-rules ()
    [(_ e)
     (letv ([(x y) e]) y)]))


(define *debug* #f)
(define-syntax peek
  (syntax-rules ()
    [(_ name args ...)
     (if *debug*
         (begin
           (printf "~s: ~s = ~s~n" name 'args args)
           ...)
         (void))]))


;; utility for error reporting
(define fatal
  (lambda (who . args)
    (printf "~s: " who)
    (for-each display args)
    (display "\n")
    (error who "")))


; foldl of Racket has a bug!
; (foldl (lambda (x y) x) 0 '(1 2 3 4))
; => 4
; Don't use it!
(define foldl2
  (lambda (f x ls)
    (cond
     [(null? ls) x]
     [else
       (foldl2 f (f x (car ls)) (cdr ls))])))


; (foldl2 + 0 '(1 2 3 4 ))



(define orf
  (lambda (a b)
    (or a b)))




(define char->string string)


(define read-file
  (lambda (filename)
    (let ([port (open-input-file filename #:mode 'text)])
      (let loop ([line (read-line port)]
                 [all ""])
        (cond
         [(eof-object? line) all]
         [else
          (loop (read-line port)
                (string-append all line "\n"))])))))



(define new-progress
  (lambda (size)
    (let* ([counter 0]
           [dots 0]
           [print-mark
            (lambda (sym)
              (display sym)
              (set! dots (+ dots 1))
              (cond
               [(= 0 (modulo dots 60))
                (display "\n")])
              (flush-output))])
      (lambda (x)
        (cond
         [(eq? x 'reset)
          (set! counter 0)
          (set! dots 0)]
         [(eq? x 'get)
          counter]
         [(string? x)
          (print-mark x)]
         [(= 0 (remainder counter size))
          (set! counter (+ x counter))
          (print-mark ".")]
         [else
          (set! counter (+ x counter))])))))



;;----------------- multi dimensional eq hash --------------------

(define hash-put!
  (lambda (hash key1 key2 v)
    (cond
     [(hash-has-key? hash key2)
      (let ([inner (hash-ref hash key2)])
        (hash-set! inner key1 v))]
     [else
      (let ([inner (make-hasheq)])
        (hash-set! inner key1 v)
        (hash-set! hash key2 inner))])))

(define hash-get
  (lambda (hash key1 key2)
    (cond
     [(hash-has-key? hash key2)
      (let ([inner (hash-ref hash key2)])
        (cond
         [(hash-has-key? inner key1)
          (hash-ref inner key1)]
         [else #f]))]
     [else #f])))


(define hash-put2!
  (lambda (hash key1 key2 v)
    (cond
     [(hash-has-key? hash key2)
      (let ([inner (hash-ref hash key2)])
        (hash-set! inner key1 v))]
     [else
      (let ([inner (make-hash)])
        (hash-set! inner key1 v)
        (hash-set! hash key2 inner))])))

(define hash-get2
  (lambda (hash key1 key2)
    (cond
     [(hash-has-key? hash key2)
      (let ([inner (hash-ref hash key2)])
        (cond
         [(hash-has-key? inner key1)
          (hash-ref inner key1)]
         [else #f]))]
     [else #f])))


(define predand
  (lambda preds
    (lambda (x)
      (cond
       [(null? preds) #t]
       [((car preds) x) 
        ((apply predand (cdr preds)) x)]
       [else #f]))))


(define predor
  (lambda preds
    (lambda (x)
      (cond
       [(null? preds) #f]
       [((car preds) x) #t]
       [else
        ((apply predor (cdr preds)) x)]))))


(define set-
  (lambda (s1 s2)
    (cond
     [(null? s1) '()]
     [(memq (car s1) s2)
      (set- (cdr s1) s2)]
     [else
      (cons (car s1) (set- (cdr s1) s2))])))



(define string-join
  (lambda (ls sep)
    (cond
     [(null? ls) ""]
     [else 
      (string-append (car ls) sep (string-join (cdr ls) sep))])))

; (string-join (list "a" "b" "c") ",")

