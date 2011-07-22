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



(load "utils.ss")

(define *debug* #f)
(define *left-recur-detection* #f)


;-------------------------------------------------------------
;                 parser combinator library
;-------------------------------------------------------------

;; s-expression settings
;; please override for other languages.
(define *delims* (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  "," ))
(define *line-comment*  (list ";"))
(define *comment-start*  "")
(define *comment-end*    "")
(define *operators*  '())
(define *quotation-marks*  '(#\"))
(define *significant-whitespaces* '())



;--------------------- data types ---------------------------
(struct Expr     (type elts start end)  #:transparent)
(struct Token    (text start end)       #:transparent)
(struct Char     (c start end)          #:transparent)
(struct Comment  (text start end)       #:transparent)
(struct Str      (s start end)          #:transparent)
(struct Newline  (start end)            #:transparent)
(struct Phantom  (start end)            #:transparent)



(define node-type
  (lambda (node)
    (and (Expr? node) (Expr-type node))))


(define get-start
  (lambda (node)
    (cond
      [(Expr? node)    (Expr-start node)]
      [(Token? node)   (Token-start node)]
      [(Char? node)    (Char-start node)]
      [(Comment? node) (Comment-start node)]
      [(Str? node)     (Str-start node)]
      [(Newline? node) (Newline-start node)]
      [(Phantom? node) (Phantom-start node)]
      [else
       (fatal 'get-start
              "unrecognized node: " node)])))


(define get-end
  (lambda (node)
    (cond
      [(Expr? node)    (Expr-end node)]
      [(Token? node)   (Token-end node)]
      [(Char? node)    (Char-end node)]
      [(Comment? node) (Comment-end node)]
      [(Str? node)     (Str-end node)]
      [(Newline? node) (Newline-end node)]
      [(Phantom? node) (Phantom-end node)]
      [else
       (fatal 'get-end
              "unrecognized node: " node)])))





;-------------------------------------------------------------
;                          scanner
;-------------------------------------------------------------

(define whitespace?  char-whitespace?)
(define alpha?       char-alphabetic?)
(define digit?       char-numeric?)


; Is char c a delim?
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
         [else
          (cond
           [(start-with-one-of s start *significant-whitespaces*)
            (values (Newline start (add1 start)) (add1 start))]

           [(whitespace? (string-ref s start))
            (scan1 s (add1 start))]

           [(start-with-one-of s start *line-comment*) ; line comment
            (let ([line-end (find-next s start
                                       (lambda (s start)
                                         (eq? (string-ref s start) #\newline)))])
              (values (Comment (substring s start line-end)
                               start (add1 line-end))
                      line-end))]

           [(start-with s start *comment-start*) ; block comment
            (let* ([line-end (find-next s start
                                        (lambda (s start)
                                          (start-with s start *comment-end*)))]
                   [end (+ line-end (string-length *comment-end*))])
              (values (Comment (substring s start end) start end) end))]

           [(find-delim s start) =>
            (lambda (delim)
              (let ([end (+ start (string-length delim))])
                (values (Token delim start end) end)))]

           [(find-operator s start) =>
            (lambda (op)
              (let ([end (+ start (string-length op))])
                (values (Token op start end) end)))]

           [(start-with-one-of s start *quotation-marks*)   ; string
            => (lambda (q) (scan-string s start q))]

           [(start-with-one-of s start (list "#\\" "?\\")) ; scheme/elisp char
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
                (values (Char (string-ref s (sub1 end)) start end) end))])]

           [else                        ; identifier or number
            (let loop ([pos start] [chars '()])
              (cond
               [(or (<= (string-length s) pos)
                    (whitespace? (string-ref s pos))
                    (find-delim s pos)
                    (find-operator s pos))
                (let ([text (list->string (reverse chars))])
                  (values (Token text start pos) pos))]
               [else
                (loop (add1 pos) (cons (string-ref s pos) chars))]))])])))
    (let loop ([start 0] [toks '()])
      (letv ([(tok newstart) (scan1 s start)])
        (cond
         [(eq? tok 'eof)
          (reverse toks)]
         [else
          (loop newstart (cons tok toks))])))))



(define scan-string
  (lambda (s start quot)
    (cond
     [(not (eq? quot (string-ref s start)))
      (error 'scan-string "string must start with quote")]
     [else
      (let loop ([next (add1 start)] [chars '()])
        (cond
         [(<= (string-length s) next)
          (error 'scan-string "reached EOF while scanning string")]
         [else
          (let ([c (string-ref s next)])
            (cond
             [(eq? c quot)
              (let ([str (list->string (reverse chars))]
                    [end (add1 next)])
                (values (Str str start end) end))]
             [(eq? c #\\)
              (cond
               [(<= (string-length s) (add1 next))
                (error 'scan-string "reached EOF while scanning string")]
               [else
                (loop (+ 2 next) (cons (string-ref s (add1 next))
                                       (cons #\\ chars)))])]
             [else
              (loop (add1 next) (cons c chars))]))]))])))





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



(define stack->string
  (lambda (stk)
    (let ([ps (map
               (lambda (x) (format "~a" (car x)))
               stk)])
      (string-join ps "\n"))))

; (display (stack->string (onstack? 'x 'y '((u . v) (x . y) (w . t)))))



(define ext
  (lambda (u v stk)
    (cond
     [(not *left-recur-detection*) stk]
     [else
      (cons (cons u v) stk)])))



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
              (values (filter (negate Phantom?) t) r)])))))))


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
              (values (filter (negate Phantom?) t) r)]
             [(null? t)
              (values (list (Expr type '()
                                  (get-start (car toks))
                                  (get-start (car toks)) ))
                      r)]
             [else
              (values (list (Expr type
                                  (filter (negate Phantom?) t)
                                  (get-start (car t))
                                  (get-end (last t))))
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
                (values (list (Phantom (get-start (car t))
                                       (get-end (last t))))
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


(define $$
  (lambda (s)
    ($pred
     (lambda (x)
       (and (Token? x) (string=? (Token-text x) s))))))


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


(define @.@
  (lambda (p sep)
    (@... p (@* (@... sep p)))))



;; ($eval (@.@ ($$ "foo") ($$ ","))
;;        (scan "foo, foo,foo "))





;-------------------------------------------------------------
;                  associaltive expressions
;-------------------------------------------------------------


;; construct left-associative infix expression
(define constr-exp-l
  (lambda (type fields)
    (let loop ([fields (cdr fields)] [ret (car fields)])
         (cond
          [(null? fields) ret]
          [else
           (let ([e (Expr type (list ret (car fields) (cadr fields))
                          (get-start ret)
                          (get-end (cadr fields)))])
             (loop (cddr fields) e))]))))


;; construct right-associative infix expression
(define constr-exp-r
  (lambda (type fields)
    (let ([fields (reverse fields)])
      (let loop ([fields (cdr fields)] [ret (car fields)])
           (cond
            [(null? fields) ret]
            [else
             (let ([e (Expr type (list (cadr fields) (car fields) ret)
                            (get-start (cadr fields))
                            (get-end ret))])
               (loop (cddr fields) e))])))))


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
           (let ([e (Expr type
                          (list ret (car ls))
                          (get-start ret)
                          (get-end (car ls)))])
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
        (Expr type
              (list (car ls) tail)
              (get-start (car ls))
              (get-end tail)))])))


;; ($eval (@prefix 'prefix $primary-expression $prefix-operator)
;;        (scan "-1"))



;;----------------- syntax extensions --------------------

(define *parse-hash* (make-hasheq))


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



;; fail if in avoid-ctx
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



;; (::= $foo
;;      (@= 'foo (@... $bar ($$ "foo"))))

;; (::? $bar $baz
;;      ($$ "bar"))

;; (::= $baz
;;      (@= 'baz (@... $bar ($$ "baz"))))


;; ($eval $bar (scan "bar foo"))
;; ($eval $foo (scan "bar foo"))
;; ($eval $baz (scan "bar baz"))           ; only this one succeeds


;; (::! $avoid-foo $foo
;;      (@= 'avoid-foo ($$ "avoid-foo")))

;; (::= $foo
;;      (@= 'foo (@... $avoid-foo ($$ "foo"))))

;; (::= $not-foo
;;      (@= 'not-foo (@... $avoid-foo ($$ "not-foo"))))


;; ($eval $foo (scan "avoid-foo foo"))     ; $avoid-foo fails only in foo
;; ($eval $not-foo (scan "avoid-foo not-foo"))





(define $eval
  (lambda (p toks)
    (set! *parse-hash* (make-hasheq))
    (letv ([(t r) ((p) toks '() '())])
      (set! *parse-hash* (make-hasheq))
      (values t r))))


(define parse1
  (lambda (p s)
    (letv ([(t r) ($eval p (filter (lambda (x) (not (Comment? x)))
                                   (scan s)))])
      t)))






;-------------------------------------------------------------
;                    testing facilities
;-------------------------------------------------------------

(define test-string
  (lambda (s)
    (letv ([(t r) ($eval $program
                         (filter (lambda (x) (not (Comment? x)))
                                 (scan s)))])
      (cond
       [(null? r) #t]
       [(not r) #f]
       [else (car r)]))))


(define test-file1
  (lambda (file)
    (printf "testing file: ~a ... " file)
    (let ([start (current-seconds)])
      (flush-output)
      (let ([res (test-string (read-file file))])
        (cond
         [(eq? #t res)
          (printf "succeed.~ntime used: ~a seconds~n"
                  (- (current-seconds) start))
          (flush-output)]
         [else
          (printf "failed at token: ~a~n" res)
          (flush-output)])))))


(define test-file
  (lambda files
    (for-each test-file1 files)))



;-------------------------- examples ---------------------------

; a parser for s-expressions

(:: $open
     (@or (@~ "(") (@~ "[")))

(:: $close
     (@or (@~ ")") (@~ "]")))

(:: $non-parens
     (@and (@! $open) (@! $close)))

(:: $parens
     (@... $open (@* $sexp) $close))

(::= $sexp 'sexp
    (@+ (@or $non-parens $parens)))

(::= $program 'program
     $sexp)


(define parse-sexp
  (lambda (s)
    (first-val ($eval $program (scan s)))))


;; (parse-sexp (read-file "paredit20.el"))
;; (parse-sexp "(lambda (x) x)")




;;-------------- direct left recursion test ---------------
;;
;; (::= $left 'left
;;   (@or (@seq $left ($$ "ok"))
;;        ($$ "ok")))

;; ($eval $left (scan "ok"))


;;---------- indirect left-recursion -------------
;;
;; (::= $left1 'left1
;;   (@seq $left2 ($$ "ok")))

;; (::= $left2 'left2
;;   (@or (@seq $left1 ($$ "ok"))
;;        ($$ "ok")))

;; ($eval $left1 (scan "ok ok"))

