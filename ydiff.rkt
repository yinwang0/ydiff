;; standalone version of ydiff which compares two Scheme programs


#lang racket

(require "utils.ss")

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




;-------------------------------------------------------------
;                       data types
;-------------------------------------------------------------
(struct Node    (start end)        #:transparent)
(struct Expr     Node (type elts)  #:transparent)
(struct Token    Node (text)       #:transparent)
(struct Comment  Node (text)       #:transparent)
(struct Str      Node (text)       #:transparent)
(struct Char     Node (text)       #:transparent)
(struct Newline  Node ()           #:transparent)
(struct Phantom  Node ()           #:transparent)


(define node-type
  (lambda (node)
    (and (Expr? node) (Expr-type node))))


(define get-start
  (lambda (node)
    (Node-start node)))


(define get-end
  (lambda (node)
    (Node-end node)))


(define get-symbol
  (lambda (node)
    (cond
     [(Token? node)
      (string->symbol (Token-text node))]
     [else #f])))


(define get-tag
  (lambda (e tag)
    (let ([matches (filter (lambda (x)
                             (and (Expr? x)
                                  (eq? (Expr-type x) tag)))
                           (Expr-elts e))])
      (cond
       [(null? matches) #f]
       [else (car matches)]))))


(define match-tags
  (lambda (e tags)
    (cond
     [(not (Expr? e)) #f]
     [(null? tags) e]
     [else
      (match-tags (get-tag e (car tags)) (cdr tags))])))




;-------------------------------------------------------------
;                          scanner
;-------------------------------------------------------------

(define whitespace?  char-whitespace?)
(define alpha?       char-alphabetic?)
(define digit?       char-numeric?)


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
     [(digit? (string-ref s 0)) #t]
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
          (values (Newline start (add1 start)) (add1 start))]

         [(whitespace? (string-ref s start))
          (scan1 s (add1 start))]

         [(start-with-one-of s start *line-comment*) ; line comment
          (let ([line-end (find-next s start
                                     (lambda (s start)
                                       (eq? (string-ref s start) #\newline)))])
            (values (Comment start (add1 line-end) (substring s start line-end))
                    line-end))]

         [(start-with s start *comment-start*) ; block comment
          (let* ([line-end (find-next s start
                                      (lambda (s start)
                                        (start-with s start *comment-end*)))]
                 [end (+ line-end (string-length *comment-end*))])
            (values (Comment start end (substring s start end)) end))]

         [(find-delim s start) =>
          (lambda (delim)
            (let ([end (+ start (string-length delim))])
              (values (Token start end delim) end)))]

         [(find-operator s start) =>
          (lambda (op)
            (let ([end (+ start (string-length op))])
              (values (Token start end op) end)))]

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
                (values (Str start end (car reg-match)) end))]))]

         ;; => (lambda (q) (scan-string s start q))

         [(start-with-one-of s start *lisp-char*) ; scheme/elisp char
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
              (values (Char start end (string-ref s (sub1 end))) end))])]

         [else                        ; identifier or number
          (let loop ([pos start] [chars '()])
            (cond
             [(or (<= (string-length s) pos)
                  (whitespace? (string-ref s pos))
                  (find-delim s pos)
                  (find-operator s pos))
              (let ([text (list->string (reverse chars))])
                (values (Token start pos text) pos))]
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
              (values (list (Expr (get-start (car toks))
                                  (get-start (car toks))
                                  type '()))
                      r)]
             [else
              (values (list (Expr (get-start (car t))
                                  (get-end (last t))
                                  type
                                  (filter (negate Phantom?) t)))
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
           (let ([e (Expr (get-start ret)
                          (get-end (cadr fields))
                          type (list ret (car fields) (cadr fields)))])
             (loop (cddr fields) e))]))))


;; helper for constructing right-associative infix expression
(define constr-exp-r
  (lambda (type fields)
    (let ([fields (reverse fields)])
      (let loop ([fields (cdr fields)] [ret (car fields)])
           (cond
            [(null? fields) ret]
            [else
             (let ([e (Expr (get-start (cadr fields))
                            (get-end ret)
                            type (list (cadr fields) (car fields) ret))])
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
           (let ([e (Expr (get-start ret)
                          (get-end (car ls))
                          type
                          (list ret (car ls)))])
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
        (Expr (get-start (car ls))
              (get-end tail)
              type
              (list (car ls) tail)))])))


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
    (letv ([(t r) ($eval p (filter (lambda (x) (not (Comment? x)))
                                   (scan s)))])
      t)))




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




;;; (load "diff.ss")

;-------------------------------------------------------------
;                      parameters
;-------------------------------------------------------------

;; The ratio of cost/size that we consider two nodes to be
;; "similar", so as to perform a heuristic move (that will
;; cut running time by a lot.) But this number should be
;; small enough otherwise everything will be considered to
;; be moves! Set to a small number for accuracy.
(define *move-ratio* 0)


;; The minimum size of a node to be considered for moves.
;; Shouldn't be too small, otherwise small deletec names
;; will appear in a very distant place!
(define *move-size* 5)


;; How long must a string be in order for us to use string-dist
;; function, which is costly when used on long strings but the most
;; accurate method to use. Currently this parameter is set to 0,
;; effective disables all LCS string comparison. This improves
;; performance while not sacrificing accuracy because the algorithm is
;; AST based.
(define *max-string-len* 0)


;; Only memoize the diff of nodes of size larger than this number.
;; This effectively reduces memory usage.
(define *memo-node-size* 2)





;-------------------------------------------------------------
;                      data types
;-------------------------------------------------------------

(struct Change (orig cur cost type) #:transparent)
(struct Tag (tag idx start) #:transparent)

(define ins?
  (lambda (c)
    (eq? 'ins (Change-type c))))

(define del?
  (lambda (c)
    (eq? 'del (Change-type c))))

(define mod?
  (lambda (c)
    (eq? 'mod (Change-type c))))



;----------------- utils for creating changes ----------------
(define ins
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change #f node size 'ins)))))


(define del
  (lambda (node)
    (let ([size (node-size node)])
      (list (Change node #f size 'del)))))


(define mod
  (lambda (node1 node2 cost)
    (list (Change node1 node2 cost 'mod))))


(define mov
  (lambda (node1 node2 cost)
    (list (Change node1 node2 cost 'mov))))


;; create a "total change". (delete node1 and insert node2)
(define total
  (lambda (node1 node2)
    (let ([size1 (node-size node1)]
          [size2 (node-size node2)])
      (values (append (del node1) (ins node2))
              (+ size1 size2)))))


(define mod->mov
  (lambda (c)
    (match c
     [(Change node1 node2 cost 'mod)
      (Change node1 node2 cost 'mov)]
     [other other])))



;;------------------ frames utils --------------------
(define deframe
  (lambda (node)
    (match node
      [(Expr _ _ 'frame elts)
       (apply append (map deframe elts))]
     [else (list node)])))


(define deframe-change
  (lambda (change)
    (cond
     [(ins? change)
      (apply append
             (map ins (deframe (Change-cur change))))]
     [(del? change)
      (apply append
             (map del (deframe (Change-orig change))))]
     [else (list change)])))


(define extract-frame
  (lambda (node1 node2 type)
    (match node1
      [(Expr start1 end1 type1 elts1)
       (let ([frame-elts (filter (lambda (x)
                                   (not (eq? x node2)))
                                 elts1)])
         (type (Expr start1 start1 'frame frame-elts)))]
      [_ fatal 'extract-frame "I only accept Expr"])))


;; (define n1 (Token "ok" 0 1))
;; (define n2 (Expr 'ok 0 2 (list n1 (Token "bar" 1 2))))
;; (map deframe-change (extract-frame n2 n1 ins))






;------------------ operations on nodes ---------------------


;; same-def? and different-def? only depend on get-name, so they need
;; not be overridden by individual languages.
(define same-def?
  (lambda (e1 e2)
    (cond
     [(not (eq? (get-type e1) (get-type e2)))
      #f]
     [else
      (let ([name1 (get-name e1)]
            [name2 (get-name e2)])
        (and name1 name2 (equal? name1 name2)))])))


(define different-def?
  (lambda (e1 e2)
    (cond
     [(not (eq? (get-type e1) (get-type e2)))
      #f]
     [else
      (let ([name1 (get-name e1)]
            [name2 (get-name e2)])
        (and name1 name2 (not (equal? name1 name2))))])))




;; whether two nodes are similar given the cost
(define similar?
  (lambda (node1 node2 c)
    (<= c (* *move-ratio* (+ (node-size node1)
                             (node-size node2))))))



;----------- node size function ------------
(define *node-size-hash* (make-hasheq))

(define node-size
  (lambda (node)
    (define memo
      (lambda (v)
        (if (> v 1)
            (hash-set! *node-size-hash* node v)
            (void))
        v))
    (cond
     [(pair? node)
      (apply + (map node-size node))]
     [(or (Token? node) (Str? node) (Char? node)) 1]
     [(Expr? node)
      (cond
       [(hash-has-key? *node-size-hash* node)
        (hash-ref *node-size-hash* node)]
       [else
        (memo (node-size (Expr-elts node)))])]
     [else 0])))


(define node-depth
  (lambda (node)
    (cond
     [(null? node) 0]
     [(pair? node)
      (apply max (map node-depth node))]
     [(Expr? node)
      (add1 (node-depth (Expr-elts node)))]
     [else 0])))


; (node-depth (parse-scheme "(lambda (x (x (y)) (y)) x)"))


(define uid
  (let ([count 1]
        [table (box '())])
    (lambda (node)
      (let ([p (assq node (unbox table))])
        (cond
         [(not p)
          (let ([id count])
            (set! count (add1 count))
            (set-box! table (cons `(,node . ,id) (unbox table)))
            id)]
         [else
          (cdr p)])))))



;; similarity string from a change
(define similarity
  (lambda (change)
    (let ([total (+ (node-size (Change-orig change))
                    (node-size (Change-cur change)))])
      (cond
       [(or (= 0 total) (= 0 (Change-cost change)))
        "100%"]
       [else
        (string-append
         (real->decimal-string
          (* 100 (- 1.0 (/ (Change-cost change) total))) 1)
         "%")]))))



;-------------------------------------------------------------
;                       diff proper
;-------------------------------------------------------------

; 2-D memoization table
(define make-table
  (lambda (dim1 dim2)
    (let ([vec (make-vector (add1 dim1))])
      (let loop ([n 0])
        (cond
         [(= n (vector-length vec)) vec]
         [else
          (vector-set! vec n (make-vector (add1 dim2) #f))
          (loop (add1 n))])))))


(define table-ref
  (lambda (t x y)
    (let ([row (vector-ref t x)])
      (vector-ref row y))))


(define table-set!
  (lambda (t x y v)
    (let ([row (vector-ref t x)])
      (vector-set! row y v))))



;---------------- string distance function -----------------

;; string distance is no longer used because string=? saffice to
;; compare strings in ASTs. Retain it here for possible later uses.
(define string-dist
  (lambda (s1 s2)
    (let* ([len1 (string-length s1)]
           [len2 (string-length s2)]
           [t (make-table len1 len2)]
           [char-dist (dist1 t s1 0 s2 0)])
      (cond
       [(= 0 (+ len1 len2)) 0]
       [else
        (/ (* 2.0 char-dist) (+ len1 len2))]))))


(define dist1
  (lambda (table s1 start1 s2 start2)
    (define memo
      (lambda (value)
        (table-set! table start1 start2 value)
        value))
    (cond
     [(table-ref table start1 start2)
      => (lambda (cached) cached)]
     [(= start1 (string-length s1))
      (memo (- (string-length s2) start2))]
     [(= start2 (string-length s2))
      (memo (- (string-length s1) start1))]
     [else
      (let* ([c1 (string-ref s1 start1)]
             [c2 (string-ref s2 start2)]
             [d0 (cond
                  [(char=? c1 c2) 0]
                  [(char=? (char-downcase c1)
                           (char-downcase c2)) 1]
                  [else 2])]
             [d1 (+ d0 (dist1 table s1 (add1 start1) s2 (add1 start2)))]
             [d2 (+ 1 (dist1 table s1 (add1 start1) s2 start2))]
             [d3 (+ 1 (dist1 table s1 start1 s2 (add1 start2)))])
        (memo (min d1 d2 d3)))])))





;--------------------- the primary diff function -------------------
(define diff-node
  (lambda (node1 node2 move?)

    (define memo
      (lambda (v1 v2)
        (and (> (node-size node1) *memo-node-size*)
             (> (node-size node2) *memo-node-size*)
             (hash-put! *diff-hash* node1 node2 (cons v1 v2)))
        (values v1 v2)))

    (define try-extract
      (lambda (changes cost)
        (cond
         [(or (not move?)
              (similar? node1 node2 cost))
          (memo changes cost)]
         [else
          (letv ([(m c) (diff-extract node1 node2 move?)])
            (cond
             [(not m)
              (memo changes cost)]
             [else
              (memo m c)]))])))


    (diff-progress 1) ;; progress bar

    (cond
     [(hash-get *diff-hash* node1 node2)
      => (lambda (cached)
           (values (car cached) (cdr cached)))]
     [(and (Char? node1) (Char? node2))
      (diff-string (char->string (Char-text node1))
                   (char->string (Char-text node2))
                   node1 node2)]
     [(and (Str? node1) (Str? node2))
      (diff-string (Str-text node1) (Str-text node2) node1 node2)]
     [(and (Comment? node1) (Comment? node2))
      (diff-string (Comment-text node1) (Comment-text node2) node1 node2)]
     [(and (Token? node1) (Token? node2))
      (diff-string (Token-text node1) (Token-text node2) node1 node2)]
     [(and (Expr? node1) (Expr? node2)
           (eq? (get-type node1) (get-type node2)))
      (letv ([(m c) (diff-list (Expr-elts node1) (Expr-elts node2) move?)])
        (try-extract m c))]
     [(and (pair? node1) (not (pair? node2)))
      (diff-list node1 (list node2) move?)]
     [(and (not (pair? node1)) (pair? node2))
      (diff-list (list node1) node2 move?)]
     [(and (pair? node1) (pair? node2))
      (diff-list node1 node2 move?)]
     [else
      (letv ([(m c) (total node1 node2)])
        (try-extract m c))])))




;; helper for nodes with string contents (Str, Comment, Token etc.)
(define diff-string
  (lambda (string1 string2 node1 node2)
    (cond
     [(or (> (string-length string1) *max-string-len*)
          (> (string-length string2) *max-string-len*))
      (cond
       [(string=? string1 string2)
        (values (mod node1 node2 0) 0)]
       [else
        (total node1 node2)])]
     [else
      (let ([cost (string-dist string1 string2)])
        (values (mod node1 node2 cost) cost))])))





;; global 2D hash for storing known diffs
(define *diff-hash* (make-hasheq))

(define diff-list
  (lambda (ls1 ls2 move?)
    (let ([ls1 (sort ls1 node-sort-fn)]
          [ls2 (sort ls2 node-sort-fn)])
      (diff-list1 (make-hasheq) ls1 ls2 move?))))


(define diff-list1
  (lambda (table ls1 ls2 move?)

    (define memo
      (lambda (v1 v2)
        (hash-put! table ls1 ls2 (cons v1 v2))
        (values v1 v2)))

    (define guess
      (lambda (ls1  ls2)
        (letv ([(m0 c0) (diff-node (car ls1) (car ls2) move?)]
               [(m1 c1) (diff-list1 table (cdr ls1) (cdr ls2) move?)]
               [cost1 (+ c0 c1)])
          (cond
           [(or (same-def? (car ls1) (car ls2))
                (and (not (different-def? (car ls1) (car ls2)))
                     (similar? (car ls1) (car ls2) c0)))
            (memo (append m0 m1) cost1)]
           [else
            (letv ([(m2 c2) (diff-list1 table (cdr ls1) ls2  move?)]
                   [(m3 c3) (diff-list1 table ls1 (cdr ls2) move?)]
                   [cost2 (+ c2 (node-size (car ls1)))]
                   [cost3 (+ c3 (node-size (car ls2)))])
              (cond
               [(<= cost2 cost3)
                (memo (append (del (car ls1)) m2) cost2)]
               [else
                (memo (append (ins (car ls2)) m3) cost3)]))]))))

    (cond
     [(hash-get table ls1 ls2)
      => (lambda (cached)
           (values (car cached) (cdr cached)))]
     [(and (null? ls1) (null? ls2))
      (values '() 0)]
     [(null? ls1)
      (let ([changes (apply append (map ins ls2))])
        (values changes (node-size ls2)))]
     [(null? ls2)
      (let ([changes (apply append (map del ls1))])
        (values changes (node-size ls1)))]
     [else
      (guess ls1 ls2)])))




;; structure extraction
(define diff-extract
  (lambda (node1 node2 move?)
    (cond
     [(or (< (node-size node1) *move-size*)
          (< (node-size node2) *move-size*))
      (values #f #f)]
     [(and (Expr? node1) (Expr? node2))
      (cond
       [(<= (node-size node1) (node-size node2))
        (let loop ([elts2 (Expr-elts node2)])
          (cond
           [(null? elts2) (values #f #f)]
           [else
            (letv ([(m0 c0) (diff-node node1 (car elts2)  move?)])
              (cond
               [(or (same-def? node1 (car elts2))
                    (similar? node1 (car elts2) c0))
                (let ([frame (extract-frame node2 (car elts2) ins)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts2))]))]))]
       [else
        (let loop ([elts1 (Expr-elts node1)])
          (cond
           [(null? elts1) (values #f #f)]
           [else
            (letv ([(m0 c0) (diff-node (car elts1) node2 move?)])
              (cond
               [(or (same-def? (car elts1) node2)
                    (similar? (car elts1) node2 c0))
                (let ([frame (extract-frame node1 (car elts1) del)])
                  (values (append m0 frame) c0))]
               [else
                (loop (cdr elts1))]))]))])]
     [else (values #f #f)])))





;-------------------------------------------------------------
;                    finding moves
;-------------------------------------------------------------

(define big-node?
  (lambda (node)
    (>= (node-size node) *move-size*)))


(define big-change?
  (lambda (c)
    (cond
     [(ins? c)
      (big-node? (Change-cur c))]
     [(del? c)
      (big-node? (Change-orig c))]
     [(mod? c)
      (or (big-node? (Change-orig c))
          (big-node? (Change-cur c)))])))


(define node-sort-fn
  (lambda (x y)
    (let ([name1 (get-name x)]
          [name2 (get-name y)])
      (cond
       [(and name1 name2)
        (string<? (symbol->string name1)
                  (symbol->string name2))]
       [(and name1 (not name2)) #t]
       [(and (not name1) name2) #f]
       [else
        (< (get-start x) (get-start y))]))))



;; iterate diff-list on the list of changes
(define closure
  (lambda (changes)
    (set! *diff-hash* (make-hasheq))
    (let loop ([changes changes] [closed '()] [count 1])
      (printf "~n[moving phase #~a] " count)
      (letv ([dels (filter (predand del? big-change?) changes)]
             [adds (filter (predand ins? big-change?) changes)]
             [rest (set- changes (append dels adds))]
             [ls1 (sort (map Change-orig dels) node-sort-fn)]
             [ls2 (sort (map Change-cur adds) node-sort-fn)]
             [(m c) (diff-list ls1 ls2 #t)]
             [new-moves (map mod->mov (filter mod? m))])
        (printf "~n~a new moves found" (length new-moves))
        (cond
         [(null? new-moves)
          (let ([all-changes (append m rest closed)])
            (apply append (map deframe-change all-changes)))]
         [else
          (let ([new-changes (filter (negate mod?) m)])
            (loop new-changes
                  (append new-moves rest closed)
                  (add1 count)))])))))




;-------------------------------------------------------------
;                      HTML generation
;-------------------------------------------------------------

;----------------- utils ----------------
(define qs
  (lambda (x)
    (let ([x (cond
              [(symbol? x) (symbol->string x)]
              [(number? x) (number->string x)]
              [(string? x) x])])
      (string-append "'" x "'"))))


(define line
  (lambda (port . s)
    (display (string-append (apply string-append s) "\n") port)))



(define change-tags
  (lambda (changes side)
    (let loop ([cs changes] [tags '()])
      (cond
       [(null? cs) tags]
       [else
        (let ([key (if (eq? side 'left)
                       (Change-orig (car cs))
                       (Change-cur (car cs)))])
          (cond
           [(or (not key)
                (= (get-start key) (get-end key)))
            (loop (cdr cs) tags)]
           [(and (Change-orig (car cs)) (Change-cur (car cs)))
            (let ([startTag (Tag (link-start (car cs) side)
                                 (get-start key) -1)]
                  [endTag (Tag "</a>" (get-end key) (get-start key))])
              (loop (cdr cs) (cons endTag (cons startTag tags))))]
           [else
            (let ([startTag (Tag (span-start (car cs) side)
                                 (get-start key) -1)]
                  [endTag (Tag "</span>" (get-end key) (get-start key))])
              (loop (cdr cs) (cons endTag (cons startTag tags))))]))]))))


(define apply-tags
  (lambda (s tags)
    (let ([tags (sort tags tag-sort-fn)])
      (let loop ([tags tags] [curr 0] [out '()])
        (cond
         [(null? tags)
          (cond
           [(< curr (string-length s))
            (loop tags (add1 curr) (cons (escape (string-ref s curr)) out))]
           [else
            (apply string-append (reverse out))])]
         [else
          (cond
           [(< curr (Tag-idx (car tags)))
            (loop tags (add1 curr) (cons (escape (string-ref s curr)) out))]
           [else
            (loop (cdr tags) curr (cons (Tag-tag (car tags)) out))])])))))



;; get the CSS class for the change
(define change-class
  (lambda (change)
    (cond
     [(and (eq? (Change-type change) 'mov)
           (> (Change-cost change) 0))
      "mc"]                                     ; move-change
     [(eq? (Change-type change) 'mov) "m"]      ; move
     [(> (Change-cost change) 0) "c"]           ; change
     [else "u"])))                              ; unchanged



(define link-start
  (lambda (change side)
    (let ([cls (change-class change)]
          [me (if (eq? side 'left)
                  (Change-orig change)
                  (Change-cur change))]
          [other (if (eq? side 'left)
                     (Change-cur change)
                     (Change-orig change))])
      (string-append
       "<a id="  (qs (uid me))
       " tid="   (qs (uid other))
       " class=" (qs cls)
       ">"))))



(define span-start
  (lambda (change side)
    (let ([cls (if (eq? 'del (Change-type change)) "d" "i")]) ; del or ins
      (string-append "<span class=" (qs cls) ">"))))



(define tag-sort-fn
  (lambda (t1 t2)
    (cond
     [(= (Tag-idx t1) (Tag-idx t2))
      (> (Tag-start t1) (Tag-start t2))]
     [else
      (< (Tag-idx t1) (Tag-idx t2))])))


(define *escape-table*
  '((#\"  .   "&quot;")
    (#\'  .    "&#39;")
    (#\<  .    "&lt;")
    (#\>  .    "&gt;")))


(define escape
  (lambda (c)
    (cond
     [(assq c *escape-table*) => cdr]
     [else (char->string c)])))



; getting the base name of a path/file name
; (base-name "mk/mk-c.scm") => mk-c
(define base-name
  (lambda (fn)
    (let loop ([i (- (string-length fn) 1)]
               [start -1]
               [end (- (string-length fn) 1)])
      (cond
       [(= i 0)
        (substring fn i end)]
       [(eq? (string-ref fn i) #\.)
        (loop (sub1 i) start i)]
       [(eq? (string-ref fn i) #\/)
        (substring fn (add1 i) end)]
       [else
        (loop (sub1 i) start end)]))))



(define html-header
  (lambda (port)
      (line port "<html>")
      (line port "<head>")
      (line port "<META http-equiv=\"Content-Type\""
                      " content=\"text/html; charset=utf-8\">")
      (line port "<LINK href=\"diff-s.css\""
                      " rel=\"stylesheet\" type=\"text/css\">")
      (line port "<script type=\"text/javascript\""
                        " src=\"nav-div.js\"></script>")
      (line port "</head>")
      (line port "<body>")))



(define html-footer
  (lambda (port)
    (line port "</body>")
    (line port "</html>")))



(define write-html
  (lambda (port text side)
    (line port (string-append "<div id=\"" side "\" class=\"src\">"))
    (line port "<pre>")
    (if (string=? side "left")
        (line port "<a id='leftstart' tid='rightstart'></a>")
        (line port "<a id='rightstart' tid='leftstart'></a>"))
    (line port text)
    (line port "</pre>")
    (line port "</div>")))



;; poor man's progress bar
(define diff-progress
  (new-progress 10000))



(define cleanup
  (lambda ()
    (set! *node-size-hash* (make-hasheq))
    (set! *diff-hash* (make-hasheq))))



;; main command
;; takes two file names and a parser
(define diff
  (lambda (file1 file2 parse)
    (cleanup)
    (letv ([start (current-seconds)]
           [s1 (read-file file1)]
           [s2 (read-file file2)]
           [node1 (parse s1)]
           [node2 (parse s2)]
           [_ (diff-progress "\nDone parsing")]
           [(changes cost) (diff-node node1 node2 #f)]
           [_ (diff-progress "\nDone diffing")]
           [changes (closure changes)]
           [_ (diff-progress "\nDone moving")]
           [_ (set! *diff-hash* (make-hasheq))]
           [ctags1 (change-tags changes 'left)]
           [ctags2 (change-tags changes 'right)]
           [tagged1 (apply-tags s1 ctags1)]
           [tagged2 (apply-tags s2 ctags2)]
           [frame-file (string-append (base-name file1) "-"
                                      (base-name file2) ".html")]
           [port (open-output-file frame-file
                                   #:mode 'text
                                   #:exists 'replace)]
           [end (current-seconds)])
      (printf "finished in ~a seconds~n" (- end start))

      (html-header port)
      (write-html port tagged1 "left")
      (write-html port tagged2 "right")
      (html-footer port)
      (close-output-port port)
      (cleanup))))



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



;-----------------------------------------
(define diff-scheme
  (lambda (file1 file2)
    (diff file1 file2 parse-scheme)))


;; process command line
(let ([args (current-command-line-arguments)])
  (diff-scheme (vector-ref args 0) (vector-ref args 1)))

