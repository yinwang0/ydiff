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

(require "structs.rkt")
(require "utils.rkt")

(provide (all-defined-out))



;----------------------------------------
;               options
;----------------------------------------

(define *left-recur-detection* #f)



;-------------------------------------------------------------
;                 parser combinator library
;-------------------------------------------------------------

;; s-expression settings
;; please override for other languages.
(define *delims* (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  ","))
(define *line-comment* (list ";"))
(define *comment-start* "")
(define *comment-end* "")
(define *operators*  '())
(define *quotation-marks* '(#\" #\'))
(define *lisp-char* (list "#\\" "?\\"))
(define *significant-whitespaces* '())


(define (set-delims x)
  (set! *delims* x))

(define (set-line-comment x)
  (set! *line-comment* x))

(define (set-comment-start x)
  (set! *comment-start* x))

(define (set-comment-end x)
  (set! *comment-end* x))

(define (set-operators x)
  (set! *operators* x))

(define (set-quotation-marks x)
  (set! *quotation-marks* x))

(define (set-lisp-char x)
  (set! *lisp-char* x))

(define (set-significant-whitespaces x)
  (set! *significant-whitespaces* x))



;-------------------------------------------------------------
;                          scanner
;-------------------------------------------------------------

(define whitespace?  char-whitespace?)
(define alpha?       char-alphabetic?)
(define digit?       char-numeric?)

(define set-alpha
  (lambda (x)
    (set! alpha? x)))


; Is char c a delimeter?
(define delim?
  (lambda (c)
    (member (char->string c) *delims*)))


(define id?
  (lambda (s)
    (cond
     [(= 0 (string-length s)) #f]
     [(or (alpha? (string-ref s 0))
          (eq? #\_ (string-ref s 0)))
      (let loop ([i 1])
        (cond
         [(>= i (string-length s)) #t]
         [else
          (let ([c (string-ref s i)])
            (cond
             [(alpha? c) (loop (add1 i))]
             [(digit? c) (loop (add1 i))]
             [(char=? c #\_) (loop (add1 i))]
             [else #f]))]))]
     [else #f])))


(define numeral?
  (lambda (s)
    (cond
     [(= 0 (string-length s)) #f]
     [(digit? (string-ref s 0)) #t
      ;; (let loop ([i 1])
      ;;   (cond
      ;;    [(>= i (string-length s)) #t]
      ;;    [else
      ;;     (let ([c (string-ref s i)])
      ;;         (cond
      ;;          [(digit? c) (loop (add1 i))]
      ;;          [(char=? c #\.) (loop (add1 i))]
      ;;          [else #f]))]))
]
     [else #f])))



(define start-with
  (lambda (s start prefix)
    (let* ([prefix-str (if (char? prefix)
                           (char->string prefix)
                           prefix)]
           [len (string-length prefix-str)])
      (cond
       [(= len 0) #f]
       [(< (string-length s) (+ start len)) #f]
       [(string=? (substring s start (+ start len)) prefix-str)
        prefix]
       [else #f]))))



(define start-with-one-of
  (lambda (s start prefixes)
    (cond
     [(null? prefixes) #f]
     [(start-with s start (car prefixes))
      (car prefixes)]
     [else
      (start-with-one-of s start (cdr prefixes))])))

; (start-with-one-of "+>>=" 0 (list ">" #\+))



(define find-next
  (lambda (s start pred)
    (cond
     [(<= (string-length s) start) #f]
     [(pred s start) start]
     [else
      (find-next s (add1 start) pred)])))



; Find the first delim that match the start of s
(define find-delim
  (lambda (s start)
    (start-with-one-of s start *delims*)))



(define find-operator
  (lambda (s start)
    (start-with-one-of s start *operators*)))

; (find-operator ">> x" 0)



(define scan
  (lambda (s)
    (define scan1
      (lambda (s start)
        (cond
         [(= start (string-length s)) (values 'eof start)]

         [(start-with-one-of s start *significant-whitespaces*)
          (values (Node 'newline start (add1 start) '() #f #f)
                  (add1 start))]

         [(whitespace? (string-ref s start))
          (scan1 s (add1 start))]

         [(start-with-one-of s start *line-comment*) ; line comment
          (let ([line-end (find-next s start
                                     (lambda (s start)
                                       (eq? (string-ref s start) #\newline)))])
            (values (Node 'comment
                          start
                          (add1 line-end)
                          (substring s start line-end)
                          #f #f)
                    line-end))]

         [(start-with s start *comment-start*) ; block comment
          (let* ([line-end (find-next s start
                                      (lambda (s start)
                                        (start-with s start *comment-end*)))]
                 [end (+ line-end (string-length *comment-end*))])
            (values (Node 'comment
                          start
                          end
                          (substring s start end)
                          #f #f)
                    end))]

         [(find-delim s start) =>
          (lambda (delim)
            (let ([end (+ start (string-length delim))])
              (values (Node 'token start end delim #f #f)
                      end)))]

         [(find-operator s start) =>
          (lambda (op)
            (let ([end (+ start (string-length op))])
              (values (Node 'token start end op #f #f)
                      end)))]

         [(start-with-one-of s start *quotation-marks*)   ; string
          (let ([reg-match (or (regexp-match (regexp "^\"(\\\\.|[^\"])*\"")
                                             s start)
                               (regexp-match (regexp "^\'(\\\\.|[^\'])*\'")
                                             s start))])
            (cond
             [(not reg-match)
              (fatal 'scan "string match error")]
             [else
              (let* ([len (string-length (car reg-match))]
                     [end (+ start len)])
                (values (Node 'str start end (car reg-match) #f #f)
                        end))]))]

         [(start-with-one-of s start *lisp-char*)         ; scheme/elisp char
          (cond
           [(<= (string-length s) (+ 2 start))
            (error 'scan-string "reached EOF while scanning char")]
           [else
            (let ([end
                   (let loop ([end (+ 3 start)])
                     (cond
                      [(or (whitespace? (string-ref s end))
                           (delim? (string-ref s end)))
                       end]
                      [else (loop (add1 end))]))])
              (values (Node 'char start end (string-ref s (sub1 end)) #f #f)
                      end))])]

         [else                        ; identifier or number
          (let loop ([pos start] [chars '()])
            (cond
             [(or (<= (string-length s) pos)
                  (whitespace? (string-ref s pos))
                  (find-delim s pos)
                  (find-operator s pos))
              (let ([text (list->string (reverse chars))])
                (values (Node 'token start pos text #f #f)
                        pos))]
             [else
              (loop (add1 pos) (cons (string-ref s pos) chars))]))])))

    (let loop ([start 0] [toks '()])
      (letv ([(tok newstart) (scan1 s start)])
        (cond
         [(eq? tok 'eof)
          (reverse toks)]
         [else
          (loop newstart (cons tok toks))])))))






;-------------------------------------------------------------
;                           parser
;-------------------------------------------------------------

(define onstack?
  (lambda (u v stk)
    (let loop ([stk stk] [trace '()])
      (cond
       [(null? stk) #f]
       [(and (eq? u (car (car stk)))
             (eq? v (cdr (car stk))))
        (reverse (cons (car stk) trace))]
       [else
        (loop (cdr stk) (cons (car stk) trace))]))))



(define ext
  (lambda (u v stk)
    (cond
     [(not *left-recur-detection*) stk]
     [else
      (cons (cons u v) stk)])))



(define stack->string
  (lambda (stk)
    (let ([ps (map
               (lambda (x) (format "~a" (car x)))
               stk)])
      (string-join ps "\n"))))

; (display (stack->string (onstack? 'x 'y '((u . v) (x . y) (w . t)))))



;; apply parser on toks, check for left-recurson if
;; *left-recur-detection* is enabled.
(define apply-check
  (lambda (parser toks stk ctx)
    (cond
     [(and *left-recur-detection*
           (onstack? parser toks stk))
      => (lambda (t)
           (fatal 'apply-check
                  "left-recursion detected     \n"
                  "parser: " parser           "\n"
                  "start token: " (car toks)  "\n"
                  "stack trace: " (stack->string t)))]
     [else
      ((parser) toks (ext parser toks stk) ctx)])))




;------------------ parser combinators --------------------
(define @seq
  (lambda ps
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([ps ps] [toks toks] [nodes '()])
          (cond
           [(null? ps)
            (values (apply append (reverse nodes)) toks)]
           [else
            (letv ([(t r) (apply-check (car ps) toks stk ctx)])
              (cond
               [(not t)
                (values #f #f)]
               [else
                (loop (cdr ps) r (cons t nodes))]))]))))))

(define set-seq
  (lambda (x)
    (set! @seq x)))



;; removes phantoms
(define @...
  (lambda ps
    (let ([parser ((apply @seq ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values #f #f)]
             [else
              (values (filter (negate phantom?) t) r)])))))))


; (((@seq)) (scan "ok"))



(define @or
  (lambda ps
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([ps ps])
             (cond
              [(null? ps)
               (values #f #f)]
              [else
               (letv ([(t r) (apply-check (car ps) toks stk ctx)])
                 (cond
                  [(not t)
                   (loop (cdr ps))]
                  [else
                   (values t r)]))]))))))


; (((@or ($$ "foo") ($$ "bar"))) (scan "bar foo"))



(define @=
  (lambda (type . ps)
    (let ([parser ((apply @seq ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values #f #f)]
             [(not type)
              (values (filter (negate phantom?) t) r)]
             [(null? t)
              (values (list (Node type
                                  (Node-start (car toks))
                                  (Node-start (car toks))
                                  '()
                                  #f #f))
                      r)]
             [else
              (values (list (Node type
                                  (Node-start (car t))
                                  (Node-end (last t))
                                  (filter (negate phantom?) t)
                                  #f #f))
                      r)])))))))



(define @*
  (lambda ps
    (let ([parser ((apply @... ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (let loop ([toks toks] [nodes '()])
            (cond
             [(null? toks)
              (values (apply append (reverse nodes)) '())]
             [else
              (letv ([(t r) (parser toks stk ctx)])
                (cond
                 [(not t)
                  (values (apply append (reverse nodes)) toks)]
                 [else
                  (loop r (cons t nodes))]))])))))))


; ($eval (@* ($$ "ok")) (scan "ok ok ok"))


;; similar to @*, but takes only one parser and will not
;; make a sequence by invoking @seq
(define @*^
  (lambda (p)
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([toks toks] [nodes '()])
          (cond
           [(null? toks)
            (values (apply append (reverse nodes)) '())]
           [else
            (letv ([(t r) ((p) toks stk ctx)])
              (cond
               [(not t)
                (values (apply append (reverse nodes)) toks)]
               [else
                (loop r (cons t nodes))]))]))))))


(define @+
  (lambda (p)
    (@... p (@* p))))

; (((@+ ($$ "ok"))) (scan "ok ok ok"))


(define @?
  (lambda ps
    (@or (apply @... ps) $none)))


; (((@? ($$ "x"))) (scan "x y z"))


;; negation - will fail if ps parses successfully.
(define @!
  (lambda ps
    (let ([parser ((apply @... ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values (list (car toks)) (cdr toks))]
             [else (values #f #f)])))))))


;; similar to @!, but takes only one parser and will not
;; make a sequence by invoking @seq
(define @!^
  (lambda (p)
    (lambda ()
      (lambda (toks stk ctx)
        (letv ([(t r) ((p) toks stk ctx)])
          (cond
           [(not t) (values (list (car toks)) (cdr toks))]
           [else (values #f #f)]))))))




(define @and
  (lambda ps
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([ps ps] [res '()])
          (cond
           [(null? ps)
            (let ([r1 (car res)])
                (values (car r1) (cadr r1)))]
           [else
            (letv ([(t r) (apply-check (car ps) toks stk ctx)])
              (cond
               [(not t)
                (values #f #f)]
               [else
                (loop (cdr ps) (cons (list t r) res))]))]))))))



; (((@and (@or ($$ "[") ($$ "{")) (@! ($$ "{")))) (scan "["))



;; parses the parsers ps normally, but "globs" the parses and doesn't
;; put them into the output.
(define $glob
  (lambda ps
    (let ([parser ((apply @... ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values #f #f)]
             [else
              (values '() r)])))))))

; (($glob ($$ "foo")) (scan "foo bar"))



;; similar to $glob, but takes only one parser and will not
;; make a sequence by invoking @seq
(define $glob^
  (lambda (p)
    (lambda ()
      (lambda (toks stk ctx)
        (letv ([(t r) ((p) toks stk ctx)])
          (cond
           [(not t) (values #f #f)]
           [else
            (values '() r)]))))))



;; A phantom is something that takes space but invisible. It is useful
;; for something whose position is important, but is meaningless to
;; show up in the AST. It is used mostly for delimeters. $phantom is
;; seldom used directly. The helper @~ creates a phantom from strings.
(define $phantom
  (lambda ps
    (let ([parser ((apply @... ps))])
      (lambda ()
        (lambda (toks stk ctx)
          (letv ([(t r) (parser toks stk ctx)])
            (cond
             [(not t) (values #f #f)]
             [else
              (cond
               [(null? t)
                (values '() r)]
               [else
                (values (list (Node 'phantom
                                    (Node-start (car t))
                                    (Node-end (last t))
                                    '()
                                    #f #f))
                        r)])])))))))




;------------------------ parsers ---------------------------

(define $fail
  (lambda ()
    (lambda (toks stk ctx)
      (values #f #f))))


(define $none
  (lambda ()
    (lambda (toks stk ctx)
      (values '() toks))))


;; succeeds if the predicate 'proc' returns true for the first token.
(define $pred
  (lambda (proc)
    (lambda ()
      (lambda (toks stk ctx)
        (cond
         [(null? toks) (values #f #f)]
         [(proc (car toks))
          (values (list (car toks)) (cdr toks))]
         [else
          (values #f #f)])))))


(define $eof
  ($glob ($pred (lambda (t) (eq? t 'eof)))))


;; literal parser for tokens. for example ($$ "for")
(define $$
  (lambda (s)
    ($pred
     (lambda (x)
       (and (token? x) (string=? (Node-elts x) s))))))


(define @_
  (lambda (s)
    ($glob ($$ s))))


(define @~
  (lambda (s)
    ($phantom ($$ s))))


(define join
  (lambda (ps sep)
    (cond
     [(null? (cdr ps)) ps]
     [else
      (cons (car ps) (cons sep (join (cdr ps) sep)))])))


;; a list of parser p separated by sep
(define @.@
  (lambda (p sep)
    (@... p (@* (@... sep p)))))



;; ($eval (@.@ ($$ "foo") ($$ ","))
;;        (scan "foo, foo, foo"))





;-------------------------------------------------------------
;                  expression parser combinators
;-------------------------------------------------------------

;; helper for constructing left-associative infix expression
(define constr-exp-l
  (lambda (type fields)
    (let loop ([fields (cdr fields)] [ret (car fields)])
         (cond
          [(null? fields) ret]
          [else
           (let ([e (Node type
                          (Node-start ret)
                          (Node-end (cadr fields))
                          (list ret (car fields) (cadr fields))
                          #f #f)])
             (loop (cddr fields) e))]))))


;; helper for constructing right-associative infix expression
(define constr-exp-r
  (lambda (type fields)
    (let ([fields (reverse fields)])
      (let loop ([fields (cdr fields)] [ret (car fields)])
           (cond
            [(null? fields) ret]
            [else
             (let ([e (Node type
                            (Node-start (cadr fields))
                            (Node-end ret)
                            (list (cadr fields) (car fields) ret)
                            #f #f)])
               (loop (cddr fields) e))])))))



;; helper for creating infix operator parser. used by @infix-left and
;; @infix-right
(define @infix
  (lambda (type p op associativity)
    (lambda ()
      (lambda (toks stk ctx)
        (let loop ([rest toks] [ret '()])
             (letv ([(tp rp) (((@seq p)) rest stk ctx)])
               (cond
                [(not tp)
                 (cond
                  [(< (length ret) 3)
                   (values #f #f)]
                  [else
                   (let ([fields (reverse (cdr ret))]
                         [constr (if (eq? associativity 'left)
                                     constr-exp-l
                                     constr-exp-r)])
                     (values (list (constr type fields))
                             (cons (car ret) rest)))])]
                [else
                 (letv ([(top rop) (((@seq op)) rp stk ctx)])
                   (cond
                    [(not top)
                     (cond
                      [(< (length ret) 2)
                       (values #f #f)]
                      [else
                       (let ([fields (reverse (append tp ret))]
                             [constr (if (eq? associativity 'left)
                                         constr-exp-l
                                         constr-exp-r)])
                         (values (list (constr type fields))
                                 rp))])]
                    [else
                     (loop rop (append (append top tp) ret))]))])))))))


(define @infix-left
  (lambda (type p op)
    (@infix type p op 'left)))


(define @infix-right
  (lambda (type p op)
    (@infix type p op 'right)))



;; ($eval (@infix-right 'binop $multiplicative-expression $additive-operator)
;;        (scan "x + y + z"))




(define @postfix
  (lambda (type p op)
    (lambda ()
      (lambda (toks stk ctx)
        (letv ([(t r) (((@... p (@+ op))) toks stk ctx)])
          (cond
           [(not t)
            (values #f #f)]
           [else
            (values (list (make-postfix type t)) r)]))))))


;; ($eval (@postfix 'ok ($$ "foo") (@= 'bar ($$ "bar")) 'ok)
;;        (scan "foo bar bar"))


(define make-postfix
  (lambda (type ls)
    (let loop ([ls (cdr ls)] [ret (car ls)])
         (cond
          [(null? ls) ret]
          [else
           (let ([e (Node type
                          (Node-start ret)
                          (Node-end (car ls))
                          (list ret (car ls))
                          #f #f)])
             (loop (cdr ls) e))]))))


(define @prefix
  (lambda (type p op)
    (lambda ()
      (lambda (toks stk ctx)
        (letv ([(t r) (((@... (@+ op) p)) toks stk ctx)])
          (cond
           [(not t)
            (values #f #f)]
           [else
            (values (list (make-prefix type t)) r)]))))))


(define make-prefix
  (lambda (type ls)
    (cond
     [(null? (cdr ls)) (car ls)]
     [else
      (let ([tail (make-prefix type (cdr ls))])
        (Node type
              (Node-start (car ls))
              (Node-end tail)
              (list (car ls) tail)
              #f #f))])))


;; ($eval (@prefix 'prefix $primary-expression $prefix-operator)
;;        (scan "-1"))



;-------------------------------------------------------------
;                   syntactic extensions
;-------------------------------------------------------------

(define *parse-hash* (make-hasheq))


;; define an unnamed parser
(define-syntax ::
  (syntax-rules ()
    [(_ name expr)
     (define name
       (lambda ()
         (lambda (toks stk ctx)
           (cond
            [(hash-get *parse-hash* name toks)
             => (lambda (p)
                  (values (car p) (cdr p)))]
            [else
             (letv ([(t r) ((expr) toks stk ctx)])
               (hash-put! *parse-hash* name toks (cons t r))
               (values t r))]))))]))



;; define a named parser
(define-syntax ::=
  (syntax-rules ()
    [(_ name type expr ...)
     (define name
       (cond
        [(symbol? type)
         (lambda ()
           (lambda (toks stk ctx)
             (cond
              [(hash-get *parse-hash* name toks)
               => (lambda (p)
                    (values (car p) (cdr p)))]
              [else
               (letv ([parser (@= type expr ...)]
                      [(t r) ((parser) toks stk (cons 'name ctx))])
                 (hash-put! *parse-hash* name toks (cons t r))
                 (values t r))])))]
        [else
         (fatal '::= "type must be a symbol, but got: " type)]))]))





;;---------------- context sensitive parsing ----------------

;; succeed only in certain context
(define-syntax ::?
  (syntax-rules ()
    [(_ name effective-ctx expr)
     (define name
       (lambda ()
         (lambda (toks stk ctx)
           (cond
            [(not (memq 'effective-ctx ctx))
             (values #f #f)]
            [(hash-get *parse-hash* name toks)
             => (lambda (p)
                  (values (car p) (cdr p)))]
            [else
             (letv ([(t r) ((expr) toks stk (cons 'name ctx))])
               (hash-put! *parse-hash* name toks t r)
               (values t r))]))))]))



;; succeed only in a context that is NOT avoid-ctx
(define-syntax ::!
  (syntax-rules ()
    [(_ name avoid-ctx expr)
     (define name
       (lambda ()
         (lambda (toks stk ctx)
           (cond
            [(memq 'avoid-ctx ctx)
             (values #f #f)]
            [(hash-get *parse-hash* name toks)
             => (lambda (p)
                  (values (car p) (cdr p)))]
            [else
             (letv ([(t r) ((expr) toks stk (cons 'name ctx))])
               (hash-put! *parse-hash* name toks t r)
               (values t r))]))))]))


;; execuate parser p on the input tokens
(define $eval
  (lambda (p toks)
    (set! *parse-hash* (make-hasheq))
    (letv ([(t r) ((p) toks '() '())])
      (set! *parse-hash* (make-hasheq))
      (values t r))))


(define parse1
  (lambda (p s)
    (letv ([(t r) ($eval p (filter (lambda (x) (not (comment? x)))
                                   (scan s)))])
      t)))
