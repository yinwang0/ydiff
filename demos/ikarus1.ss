;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(library (ikarus compiler)
  (export compile-core-expr-to-port 
          assembler-output
          current-primitive-locations eval-core)
  (import 
    (rnrs hashtables)
    (ikarus system $fx)
    (ikarus system $pairs)
    (only (ikarus system $codes) $code->closure)
    (only (ikarus system $structs) $struct-ref $struct/rtd?)
    (except (ikarus)
        fasl-write
        compile-core-expr-to-port assembler-output
        current-primitive-locations eval-core)
    (ikarus fasl write)
    (ikarus intel-assembler))


(define-syntax struct-case
  (lambda (x)
    (define (enumerate fld* i)
      (syntax-case fld* ()
        [() #'()]
        [(x . x*) 
         (with-syntax ([i i] [i* (enumerate #'x* (fx+ i 1))])
           #'(i . i*))]))
    (define (generate-body ctxt cls*)
      (syntax-case cls* (else)
        [() (with-syntax ([x x]) #'(error #f "unmatched " v 'x))]
        [([else b b* ...])  #'(begin b b* ...)]
        [([(rec-name rec-field* ...) b b* ...] . rest) (identifier? #'rec-name)
         (with-syntax ([altern (generate-body ctxt #'rest)]
                       [(id* ...) (enumerate #'(rec-field* ...) 0)]
                       [rtd #'(type-descriptor rec-name)])
          #'(if ($struct/rtd? v rtd)
                (let ([rec-field* ($struct-ref v id*)] ...)
                  b b* ...)
                altern))]))
    (syntax-case x ()
      [(_ expr cls* ...)
       (with-syntax ([body (generate-body #'_ #'(cls* ...))])
         #'(let ([v expr]) body))])))




(define (remq1 x ls)
  (cond
    [(null? ls) '()]
    [(eq? x (car ls)) (cdr ls)]
    [else
     (let ([t (remq1 x (cdr ls))])
       (cond
         [(eq? t (cdr ls)) ls]
         [else (cons (car ls) t)]))]))

(define (singleton x) (list x))

(define (union s1 s2)
  (define (add* s1 s2)
    (cond
     [(null? s1) s2]
     [else (add (car s1) (add* (cdr s1) s2))]))
  (define (add x s)
    (cond
     [(memq x s) s]
     [else (cons x s)]))
  (cond
   [(null? s1) s2]
   [(null? s2) s1]
   [else (add* s1 s2)]))

(define (difference s1 s2)
  (define (rem* s1 s2)
    (cond
     [(null? s1) s2]
     [else (remq1 (car s1) (rem* (cdr s1) s2))]))
  (cond
   [(null? s1) '()]
   [(null? s2) s1]
   [else (rem* s2 s1)]))


  
(define-struct constant (value))
(define-struct code-loc (label))
(define-struct foreign-label (label))
(define-struct var 
   (name assigned referenced 
         reg-conf frm-conf var-conf reg-move frm-move var-move
         loc index global-loc))
(define-struct cp-var (idx))
(define-struct frame-var (idx))
(define-struct new-frame (base-idx size body))
(define-struct save-cp (loc))
(define-struct eval-cp (check body))
(define-struct return (value))
(define-struct call-cp
  (call-convention label save-cp? rp-convention base-idx arg-count live-mask))
(define-struct tailcall-cp (convention label arg-count))
(define-struct primcall (op arg*))
(define-struct primref (name))
(define-struct conditional (test conseq altern))
(define-struct interrupt-call (test handler))
(define-struct bind (lhs* rhs* body))
(define-struct recbind (lhs* rhs* body))
(define-struct rec*bind (lhs* rhs* body))
(define-struct fix (lhs* rhs* body))

(define-struct seq (e0 e1))
(define-struct case-info (label args proper))
(define-struct clambda-case (info body))
(define-struct clambda (label cases cp free name))
(define-struct closure (code free*))
(define-struct funcall (op rand*))
(define-struct jmpcall (label op rand*))
(define-struct forcall (op rand*))
(define-struct codes (list body))
(define-struct assign (lhs rhs))
(define-struct mvcall (producer consumer))



(define-struct shortcut (body handler))

(define-struct fvar (idx))
(define-struct object (val))
(define-struct locals (vars body))
(define-struct nframe (vars live body))
(define-struct nfv (conf loc var-conf frm-conf nfv-conf))
(define-struct ntcall (target value args mask size))
(define-struct asm-instr (op dst src))
(define-struct disp (s0 s1))

(define mkfvar
  (let ([cache '()])
    (lambda (i)
      (cond
        [(fixnum? i)
         (cond
           [(assv i cache) => cdr]
           [else
            (let ([fv (make-fvar i)])
              (set! cache (cons (cons i fv) cache))
              fv)])]
        [else (error 'mkfvar "not a fixnum" i)]))))

(define (unique-var x)
  (make-var (gensym x) #f #f #f #f #f #f #f #f #f #f #f))

(define (recordize x)
  (define *cookie* (gensym))
  (define (gen-fml* fml*)
    (cond
      [(pair? fml*)
       (let ([v (unique-var (car fml*))])
         (putprop (car fml*) *cookie* v)
         (cons v (gen-fml* (cdr fml*))))]
      [(symbol? fml*)
       (let ([v (unique-var fml*)])
         (putprop fml* *cookie* v)
         v)]
      [else '()]))
  (define (ungen-fml* fml*)
    (cond
      [(pair? fml*)
       (remprop (car fml*) *cookie*)
       (ungen-fml* (cdr fml*))]
      [(symbol? fml*)
       (remprop fml* *cookie*)]))
  (define (properize fml*)
    (cond
      [(pair? fml*)
       (cons (car fml*) (properize (cdr fml*)))]
      [(null? fml*) '()]
      [else (list fml*)]))
  (define (quoted-sym x)
    (if (and (list? x)
             (fx= (length x) 2)
             (eq? 'quote (car x))
             (symbol? (cadr x)))
        (cadr x)
        (error 'quoted-sym "not a quoted symbol" x)))
  (define (quoted-string x)
    (if (and (list? x)
             (fx= (length x) 2)
             (eq? 'quote (car x))
             (string? (cadr x)))
        (cadr x)
        (error 'quoted-string "not a quoted string" x)))
  (define (Var x)
    (or (getprop x *cookie*) 
        (error 'recordize "unbound" x)))
  (define (lexical x) 
    (getprop x *cookie*))
  (define (get-fmls x args) 
    (define (matching? fmls args)
      (cond
        [(null? fmls) (null? args)]
        [(pair? fmls) (and (pair? args) (matching? (cdr fmls) (cdr args)))]
        [else #t]))
    (cond
      [(and (pair? x) (eq? (car x) 'case-lambda))
       (let f ([cls* (cdr x)])
         (cond
           [(null? cls*) '()]
           [(matching? (caar cls*) args) 
            (caar cls*)]
           [else (f (cdr cls*))]))]
      [else '()]))
  (define (make-global-set! lhs rhs)
    (make-funcall (make-primref '$init-symbol-value!)
      (list (make-constant lhs) rhs)))
  (define (E x ctxt)
    (cond
      [(pair? x)
       (case (car x)
         [(quote) (make-constant (cadr x))]
         [(if) 
          (make-conditional 
            (E (cadr x) #f)
            (E (caddr x) ctxt)
            (E (cadddr x) ctxt))]
         [(set!)
          (let ([lhs (cadr x)] [rhs (caddr x)])
            (cond
              [(lexical lhs) => 
               (lambda (var) 
                 (make-assign var (E rhs lhs)))]
              [else (make-global-set! lhs (E rhs lhs))]))] 
         [(begin)
          (let f ([a (cadr x)] [d (cddr x)])
            (cond
              [(null? d) (E a ctxt)]
              [else
               (make-seq (E a #f) (f (car d) (cdr d)))]))]
         [(letrec)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [rhs* (map cadr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (let ([expr (make-recbind nlhs* (map E rhs* lhs*) (E body ctxt))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(letrec*)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [rhs* (map cadr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (let ([expr (make-rec*bind nlhs* (map E rhs* lhs*) (E body ctxt))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(library-letrec*)
          (let ([bind* (cadr x)] [body (caddr x)])
            (let ([lhs* (map car bind*)]
                  [loc* (map cadr bind*)]
                  [rhs* (map caddr bind*)])
              (let ([nlhs* (gen-fml* lhs*)])
                (for-each 
                  (lambda (lhs loc) 
                    (set-var-global-loc! lhs loc))
                  nlhs* loc*)
                (let ([expr (make-rec*bind nlhs* (map E rhs* lhs*)
                               (let f ([lhs* nlhs*] [loc* loc*])
                                 (cond
                                   [(null? lhs*) (E body ctxt)]
                                   [(not (car loc*)) (f (cdr lhs*) (cdr loc*))]
                                   [else
                                    (make-seq 
                                      (make-global-set! (car loc*) (car lhs*))
                                      (f (cdr lhs*) (cdr loc*)))])))])
                  (ungen-fml* lhs*)
                  expr))))]
         [(case-lambda)
          (let ([cls*
                 (map
                   (lambda (cls)
                     (let ([fml* (car cls)] [body (cadr cls)])
                       (let ([nfml* (gen-fml* fml*)])
                         (let ([body (E body #f)])
                           (ungen-fml* fml*)
                           (make-clambda-case 
                             (make-case-info
                               (gensym)
                               (properize nfml*) 
                               (list? fml*)) 
                             body)))))
                   (cdr x))])
            (make-clambda (gensym) cls* #f #f ctxt))]
         [(lambda) 
          (E `(case-lambda ,(cdr x)) ctxt)]
         [(foreign-call)
          (let ([name (quoted-string (cadr x))] [arg* (cddr x)])
            (make-forcall name (map (lambda (x) (E x #f)) arg*)))]
         [(primitive)
          (let ([var (cadr x)])
            (make-primref var))]
         [else
          (let ([names (get-fmls (car x) (cdr x))])
            (make-funcall 
              (E (car x) #f) 
              (let f ([arg* (cdr x)] [names names])
                (cond
                  [(pair? names)
                   (cons 
                     (E (car arg*) (car names))
                     (f (cdr arg*) (cdr names)))]
                  [else
                   (map (lambda (x) (E x #f)) arg*)]))))])]
      [(symbol? x)
       (or (lexical x) 
           (make-funcall 
             (make-primref 'top-level-value) 
             (list (make-constant x))))]
      [else (error 'recordize "invalid expression" x)]))
  (E x #f))

(define (unparse x)
  (define (E-args proper x)
    (if proper 
        (map E x)
        (let f ([a (car x)] [d (cdr x)])
          (cond
            [(null? d) (E a)]
            [else (cons (E a) (f (car d) (cdr d)))]))))
  (define (E x)
    (struct-case x
      [(constant c) `(quote ,c)]
      [(code-loc x) `(code-loc ,x)]
      [(var x) (string->symbol (format "v:~a" x))]
      [(primref x) x]
      [(conditional test conseq altern) 
       `(if ,(E test) ,(E conseq) ,(E altern))]
      [(interrupt-call e0 e1)
       `(interrupt-call ,(E e0) ,(E e1))]
      [(primcall op arg*) `(,op . ,(map E arg*))]
      [(bind lhs* rhs* body) 
       `(let ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(recbind lhs* rhs* body) 
       `(letrec ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(rec*bind lhs* rhs* body) 
       `(letrec* ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      ;[(library-recbind lhs* loc* rhs* body) 
      ; `(letrec ,(map (lambda (lhs loc rhs) (list (E lhs) loc (E rhs))) 
      ;                lhs* loc* rhs*)
      ;    ,(E body))]
      [(fix lhs* rhs* body) 
       `(fix ,(map (lambda (lhs rhs) (list (E lhs) (E rhs))) lhs* rhs*)
          ,(E body))]
      [(seq e0 e1) 
       (let ()
         (define (f x ac)
           (struct-case x
             [(seq e0 e1) (f e0 (f e1 ac))]
             [else (cons (E x) ac)]))
         (cons 'begin (f e0 (f e1 '()))))]
      [(clambda-case info body)
       `(,(E-args (case-info-proper info) (case-info-args info))
          ,(E body))]
      [(clambda g cls* cp free)
       `(,g (case-lambda . ,(map E cls*)))]
      [(clambda label clauses free)
       `(code ,label . ,(map E clauses))]
      [(closure code free*)
       `(closure ,(E code) ,(map E free*))]
      [(codes list body)
       `(codes ,(map E list)
          ,(E body))]
      [(funcall rator rand*) `(funcall ,(E rator) . ,(map E rand*))]
      [(jmpcall label rator rand*)
       `(jmpcall ,label ,(E rator) . ,(map E rand*))]
      [(forcall rator rand*) `(foreign-call ,rator . ,(map E rand*))]
      [(assign lhs rhs) `(set! ,(E lhs) ,(E rhs))]
      [(return x) `(return ,(E x))]
      [(new-frame base-idx size body)
       `(new-frame [base: ,base-idx]
                   [size: ,size]
          ,(E body))]
      [(frame-var idx) 
       (string->symbol (format "fv.~a" idx))]
      [(cp-var idx) 
       (string->symbol (format "cp.~a" idx))]
      [(save-cp expr)
       `(save-cp ,(E expr))]
      [(eval-cp check body)
       `(eval-cp ,check ,(E body))]
      [(call-cp call-convention label save-cp? rp-convention base-idx arg-count live-mask)
       `(call-cp [conv: ,call-convention]
                 [label: ,label]
                 [rpconv: ,(if (symbol? rp-convention)
                               rp-convention
                               (E rp-convention))]
                 [base-idx: ,base-idx]
                 [arg-count: ,arg-count]
                 [live-mask: ,live-mask])]
      [(tailcall-cp convention label arg-count)
       `(tailcall-cp ,convention ,label ,arg-count)]
      [(foreign-label x) `(foreign-label ,x)]
      [(mvcall prod cons) `(mvcall ,(E prod) ,(E cons))]
      [(fvar idx) (string->symbol (format "fv.~a" idx))]
      [(nfv idx) 'nfv]
      [(locals vars body) `(locals ,(map E vars) ,(E body))]
      [(asm-instr op d s)
       `(asm ,op ,(E d) ,(E s))]
      [(disp s0 s1)
       `(disp ,(E s0) ,(E s1))]
      [(nframe vars live body) `(nframe ;[vars: ,(map E vars)]
                                        ;[live: ,(map E live)]
                                  ,(E body))]
      [(shortcut body handler)
       `(shortcut ,(E body) ,(E handler))]
      [(ntcall target valuw args mask size)
       `(ntcall ,target ,size)]
      [else
       (if (symbol? x) 
           x
           "#<unknown>")]))
  (E x))

(define open-mvcalls (make-parameter #t))

(define (optimize-direct-calls x)
  (define who 'optimize-direct-calls)
  (define (make-conses ls)
    (cond
      [(null? ls) (make-constant '())]
      [else 
       (make-funcall (make-primref 'cons) 
         (list (car ls) (make-conses (cdr ls))))]))      
  (define (properize lhs* rhs*)
    (cond
      [(null? lhs*) (error who "improper improper")]
      [(null? (cdr lhs*)) 
       (list (make-conses rhs*))]
      [else (cons (car rhs*) (properize (cdr lhs*) (cdr rhs*)))]))
  (define (inline-case cls rand*)
    (struct-case cls
      [(clambda-case info body)
       (struct-case info
         [(case-info label fml* proper)
          (if proper
              (and (fx= (length fml*) (length rand*))
                   (make-bind fml* rand* body))
              (and (fx<= (length fml*) (length rand*))
                   (make-bind fml* (properize fml* rand*) body)))])]))
  (define (try-inline cls* rand* default)
    (cond
      [(null? cls*) default]
      [(inline-case (car cls*) rand*)]
      [else (try-inline (cdr cls*) rand* default)]))
  (define (inline rator rand*)
    (define (valid-mv-consumer? x)
      (struct-case x
        [(clambda L cases F)
         (and (fx= (length cases) 1)
              (struct-case (car cases)
                [(clambda-case info body)
                 (struct-case info
                   [(case-info L args proper) proper])]))]
        [else #f]))
    (define (single-value-consumer? x)
      (struct-case x
        [(clambda L cases F)
         (and (fx= (length cases) 1)
              (struct-case (car cases)
                [(clambda-case info body)
                 (struct-case info
                   [(case-info L args proper)
                    (and proper (fx= (length args) 1))])]))]
        [else #f])) 
    (define (valid-mv-producer? x)
      (struct-case x
        [(funcall) #t]
        [(conditional) #f]
        [(bind lhs* rhs* body) (valid-mv-producer? body)]
        [else #f] ;; FIXME BUG
        ))
    (struct-case rator
      [(clambda g cls*)
       (try-inline cls* rand*
          (make-funcall rator rand*))]
      [(primref op)
       (case op
         ;;; FIXME HERE
         [(call-with-values)
          (cond
            [(and (open-mvcalls) (fx= (length rand*) 2))
             (let ([producer (inline (car rand*) '())] 
                   [consumer (cadr rand*)])
               (cond
                 [(single-value-consumer? consumer)
                  (inline consumer (list producer))]
                 [(and (valid-mv-consumer? consumer)
                       (valid-mv-producer? producer))
                  (make-mvcall producer consumer)]
                 [else 
                  (make-funcall rator rand*)]))]
            [else
             (make-funcall rator rand*)])]
         [else
          (make-funcall rator rand*)])]
      [else (make-funcall rator rand*)]))
  (define (Expr x)
    (struct-case x
      [(constant) x]
      [(var) x]
      [(primref) x]
      [(bind lhs* rhs* body)
       (make-bind lhs* (map Expr rhs*) (Expr body))]
      [(recbind lhs* rhs* body)
       (make-recbind lhs* (map Expr rhs*) (Expr body))]
      [(rec*bind lhs* rhs* body)
       (make-rec*bind lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional 
         (Expr test)
         (Expr conseq)
         (Expr altern))]
      [(seq e0 e1) 
       (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (make-clambda-case info (Expr body))]))
              cls*)
         cp free name)]
      [(funcall rator rand*)
       (inline (Expr rator) (map Expr rand*))]
      [(forcall rator rand*) 
       (make-forcall rator (map Expr rand*))]
      [(assign lhs rhs)
       (make-assign lhs (Expr rhs))]
      ;[(library-recbind lhs* loc* rhs* body)
      ; (make-library-recbind lhs* loc* (map Expr rhs*) (Expr body))]
      [else (error who "invalid expression" (unparse x))]))
  (Expr x))


(define simple-primitives 
  ;;; primitives that are side-effect-free
  ;;; FIXME: surely something must go here, no?
  '())

(define (optimize-letrec x)
  (define who 'optimize-letrec)
  (define (extend-hash lhs* h ref)
    (for-each (lambda (lhs) (hashtable-set! h lhs #t)) lhs*)
    (lambda (x)
      (unless (hashtable-ref h x #f)
        (hashtable-set! h x #t)
        (ref x))))
  (define (E* x* ref comp)
    (cond
      [(null? x*) '()]
      [else
       (cons (E (car x*) ref comp)
             (E* (cdr x*) ref comp))]))  
  (define (do-rhs*-old i lhs* rhs* ref comp vref vcomp)
    (cond
      [(null? rhs*) '()]
      [else
       (let ([h (make-eq-hashtable)])
         (let ([ref
                (lambda (x)
                  (unless (hashtable-ref h x #f)
                    (hashtable-set! h x #t)
                    (ref x)
                    (when (memq x lhs*)
                      (vector-set! vref i #t))))]
               [comp
                (lambda ()
                  (vector-set! vcomp i #t)
                  (comp))])
           (cons (E (car rhs*) ref comp)
                 (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp))))])) 
  (define (do-rhs* i lhs* rhs* ref comp vref vcomp)
    (cond
      [(null? rhs*) '()]
      [else
       (let ([h (make-eq-hashtable)]
             [rest (do-rhs* (fxadd1 i) lhs* (cdr rhs*) ref comp vref vcomp)])
         (let ([ref
                (lambda (x)
                  (unless (hashtable-ref h x #f)
                    (hashtable-set! h x #t)
                    (ref x)
                    (when (memq x lhs*)
                      (vector-set! vref i #t))))]
               [comp
                (lambda ()
                  (vector-set! vcomp i #t)
                  (comp))])
           (cons (E (car rhs*) ref comp) rest)))]))
  (define (partition-rhs* i lhs* rhs* vref vcomp)
    (cond
      [(null? lhs*) (values '() '() '() '() '() '())]
      [else
       (let-values 
         ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
           (partition-rhs* (fxadd1 i) (cdr lhs*) (cdr rhs*) vref vcomp)]
          [(lhs rhs) (values (car lhs*) (car rhs*))])
         (cond
           [(var-assigned lhs) 
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [(clambda? rhs)
            (values slhs* srhs* (cons lhs llhs*) (cons rhs lrhs*) clhs* crhs*)]
           [(or (vector-ref vref i) (vector-ref vcomp i))
            (values slhs* srhs* llhs* lrhs* (cons lhs clhs*) (cons rhs crhs*))]
           [else
            (values (cons lhs slhs*) (cons rhs srhs*) llhs* lrhs* clhs* crhs*)]
           ))]))
  (define (do-recbind lhs* rhs* body ref comp letrec?) 
    (let ([h (make-eq-hashtable)]
          [vref (make-vector (length lhs*) #f)]
          [vcomp (make-vector (length lhs*) #f)])
      (let* ([ref (extend-hash lhs* h ref)]
             [body (E body ref comp)])
        (let ([rhs* (do-rhs* 0 lhs* rhs* ref comp vref vcomp)])
          (let-values ([(slhs* srhs* llhs* lrhs* clhs* crhs*)
                        (partition-rhs* 0 lhs* rhs* vref vcomp)])
            ;(unless (null? clhs*)
            ;  (printf "CLHS* = ~s\n" (map unparse clhs*)))
            (let ([void* (map (lambda (x) (make-constant (void))) clhs*)])
              (make-bind slhs* srhs*
                (make-bind clhs* void*
                  (make-fix llhs* lrhs*
                    (if letrec?
                        (let ([t* (map (lambda (x) (unique-var 'tmp)) clhs*)])
                          (make-bind t* crhs*
                            (build-assign* clhs* t* body)))
                        (build-assign* clhs* crhs* body)))))))))))
  (define (build-assign* lhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (make-seq
         (make-assign (car lhs*) (car rhs*))
         (build-assign* (cdr lhs*) (cdr rhs*) body))]))
  (define (E x ref comp)
    (struct-case x
      [(constant) x]
      [(var) (ref x) x]
      [(assign lhs rhs)
       (set-var-assigned! lhs #t)
       (ref lhs)
       (make-assign lhs (E rhs ref comp))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let ([rhs* (E* rhs* ref comp)])
         (let ([h (make-eq-hashtable)])
           (let ([body (E body (extend-hash lhs* h ref) comp)])
             (make-bind lhs* rhs* body))))]
      [(recbind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #t))]
      [(rec*bind lhs* rhs* body)
       (if (null? lhs*)
           (E body ref comp)
           (do-recbind lhs* rhs* body ref comp #f))] 
      [(conditional e0 e1 e2)
       (make-conditional (E e0 ref comp) (E e1 ref comp) (E e2 ref comp))]
      [(seq e0 e1) (make-seq (E e0 ref comp) (E e1 ref comp))]
      [(clambda g cls* cp free name)
       (make-clambda g
         (map (lambda (x)
                (struct-case x
                  [(clambda-case info body)
                   (let ([h (make-eq-hashtable)])
                     (let ([body (E body (extend-hash (case-info-args info) h ref) void)])
                       (make-clambda-case info body)))]))
              cls*)
         cp free name)]
      [(funcall rator rand*)
       (let ([rator (E rator ref comp)] [rand* (E* rand* ref comp)])
         (struct-case rator
           [(primref op)
            (unless (memq op simple-primitives)
              (comp))]
           [else
            (comp)])
         (make-funcall rator rand*))]
      [(mvcall p c)
       (let ([p (E p ref comp)] [c (E c ref comp)])
         (comp)
         (make-mvcall p c))]
      [(forcall rator rand*) 
       (make-forcall rator (E* rand* ref comp))]
      [else (error who "invalid expression" (unparse x))]))
  (E x (lambda (x) (error who "free var found" x))
       void))



(define (uncover-assigned/referenced x)
  (define who 'uncover-assigned/referenced)
  (define (Expr* x*)
    (for-each Expr x*))
  (define (init-var x)
    (set-var-assigned! x #f)
    (set-var-referenced! x #f))
  (define (Expr x)
    (struct-case x
      [(constant) (void)]
      [(var) (set-var-referenced! x #t)]
      [(primref) (void)]
      [(bind lhs* rhs* body)
       (for-each init-var lhs*)
       (begin (Expr body) (Expr* rhs*))]
      [(fix lhs* rhs* body)
       (for-each init-var lhs*)
       (Expr* rhs*)
       (Expr body)
       (when (ormap var-assigned lhs*)
         (error who "a fix lhs is assigned"))]
      [(conditional test conseq altern)
       (begin (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (begin (Expr e0) (Expr e1))]
      [(clambda g cls*)
       (for-each
         (lambda (cls)
           (struct-case cls
             [(clambda-case info body)
              (for-each init-var (case-info-args info))
              (Expr body)]))
         cls*)]
      [(primcall rator rand*) (Expr* rand*)]
      [(funcall rator rand*)
       (begin (Expr rator) (Expr* rand*))]
      [(mvcall p c) (begin (Expr p) (Expr c))]
      [(forcall rator rand*) (Expr* rand*)]
      [(assign lhs rhs)
       (set-var-assigned! lhs #t)
       (Expr rhs)]
      [else (error who "invalid expression" (unparse x))]))
  (Expr x)
  x)


#|FIXME:missing-optimizations
  111 cadr
  464 $record/rtd?
  404 memq
  249 map
  114 not
  451 car
  224 syntax-error
  248 $syntax-dispatch
  237 pair?
  125 length
  165 $cdr
  137 $car
  805 $record-ref
  181 fixnum?
  328 null?
  136 fx-
  207 eq?
  153 call-with-values
  165 values
  336 apply
  384 cdr
  898 cons
  747 error
  555 void
  645 list
|#


;;; FIXME URGENT: should handle (+ x k), (- x k) where k is a fixnum
;;;               also fx+, fx-
(module (optimize-primcall)
  (define (optimize-primcall ctxt op rand*)
    (cond
      [(getprop op *cookie*) =>
       (lambda (proc)
         (proc ctxt op rand* 
               (lambda () 
                 (make-funcall (make-primref op) rand*))))]
      [else
       (make-funcall (make-primref op) rand*)]))
  (define (constant-value x k) 
    (struct-case x 
      [(constant t) (k t)] ; known
      [(bind lhs* rhs* body) (constant-value body k)]
      [(fix lhs* rhs* body) (constant-value body k)]
      [(seq e0 e1) (constant-value e1 k)]
      [else #f]))
  (define (mk-seq e0 e1)  ;;; keep e1 seq-free.
    (cond
      [(and (primcall? e0) (eq? (primcall-op e0) 'void)) e1]
      [(or (constant? e0) (primref? e0)) e1]
      [(seq? e1)
       (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))]
      [else
       (make-seq e0 e1)]))
  (define (equable? x)
    (if (number? x) (fixnum? x) #t))
  (define *cookie* (gensym "optimizer-cookie"))
  (define-syntax set-cases
    (syntax-rules ()
      [(_ ctxt op rand* giveup 
          [(op** ...) b* b** ...] ...)
       (begin
         (let ([p (lambda (ctxt op rand* giveup) b* b** ...)])
           (putprop 'op** *cookie* p) ...
           (void)) ...)]))
  (set-cases ctxt op rand* giveup
    [(eq?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or
                  (constant-value a0
                    (lambda (x0)
                      (constant-value a1
                        (lambda (x1)
                          (mk-seq (mk-seq a0 a1)
                            (make-constant (eq? x0 x1)))))
                  (and (eq? ctxt 'e)
                       (mk-seq a0 a1)))))))
         (giveup))] 
    [(eqv?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or
                  (constant-value a0
                    (lambda (x0)
                      (or (constant-value a1
                            (lambda (x1)
                              (mk-seq (mk-seq a0 a1)
                                (make-constant (eqv? x0 x1)))))
                          (and (equable? x0)
                               (optimize-primcall ctxt 'eq? rand*)))))
                  (constant-value a1
                    (lambda (x1)
                      (and (equable? x1)
                           (optimize-primcall ctxt 'eq? rand*))))
                  (and (eq? ctxt 'e)
                       (mk-seq a0 a1)))))
         (giveup))]
    [(memv)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (constant-value a1
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'e) (mk-seq a0 a1)]
                      [(constant-value a0
                         (lambda (x)
                           (mk-seq (mk-seq a0 a1)
                             (case ctxt
                               [(v) (make-constant (memv x ls))]
                               [else (make-constant
                                       (if (memv x ls) #t #f))]))))]
                      [(andmap equable? ls)
                       (optimize-primcall ctxt 'memq rand*)]
                      [(fx= (length ls) 1)
                       (mk-seq a1
                         (optimize-primcall ctxt 'eqv?
                            (list a0 (make-constant (car ls)))))]
                      [else #f])))))
         (giveup))]
    [(memq) 
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (constant-value a1
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'e) (make-seq a0 a1)]
                      [(constant-value a0
                         (lambda (x)
                           (mk-seq (mk-seq a0 a1)
                             (case ctxt
                               [(v) (make-constant (memq x ls))]
                               [else (make-constant
                                       (if (memq x ls) #t #f))]))))]
                      [(fx= (length ls) 1)
                       (mk-seq a1
                         (optimize-primcall ctxt 'eq?
                           (list a0 (make-constant (car ls)))))]
                      [else (make-funcall (make-primref '$memq) rand*)])))))
         (giveup))]
    [(length) 
     (or (and (fx= (length rand*) 1)
              (let ([a0 (car rand*)])
                (constant-value a0
                  (lambda (ls)
                    (cond
                      [(not (list? ls)) #f]
                      [(eq? ctxt 'v) (make-constant (length ls))]
                      [(eq? ctxt 'e) a0]
                      [else (mk-seq a0 (make-constant #t))])))))
         (giveup))] 
    [(list vector)
     (case ctxt
       [(v) 
        (if (null? rand*) 
            (make-constant 
              (case op
                [(list)    '()]
                [else     '#()]))
            (giveup))]
       [else
        (if (null? rand*)
            (make-constant #t)
            (let f ([a (car rand*)] [d (cdr rand*)])
              (cond
                [(null? d) (mk-seq a (make-constant #t))]
                [else
                 (f (mk-seq a (car d)) (cdr d))])))])]
    [(cons*)
     (case ctxt
       [(e) 
        (cond
          [(null? rand*) (giveup)]
          [else
           (let f ([a (car rand*)] [d (cdr rand*)])
             (cond
               [(null? d) a]
               [else (f (mk-seq a (car d)) (cdr d))]))])]
       [(p) 
        (cond
          [(null? rand*) (giveup)]
          [(null? (cdr rand*)) 
           (let ([a (car rand*)])
             (or (constant-value a
                   (lambda (v)
                     (mk-seq a (make-constant (if v #t #f)))))
                 a))]
          [else 
           (let f ([a (car rand*)] [d (cdr rand*)])
             (cond
               [(null? d) (mk-seq a (make-constant #t))]
               [else (f (mk-seq a (car d)) (cdr d))]))])]
       [else
        (cond
          [(null? rand*) (giveup)]
          [(null? (cdr rand*)) (car rand*)]
          [else (giveup)])])] 
    [(cons)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (case ctxt
                  [(e) (mk-seq a0 a1)]
                  [(p) (mk-seq (mk-seq a0 a1) (make-constant #t))]
                  [else (giveup)])))
         (giveup))]
    [($struct-ref $struct/rtd?)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (case ctxt
                  [(e) (mk-seq a0 a1)]
                  [else 
                   (or (constant-value a1
                         (lambda (n1)
                           (mk-seq a1
                             (make-funcall (make-primref op)
                                (list a0 (make-constant n1))))))
                       (make-funcall (make-primref op) rand*))])))
         (error 'optimize "invalid operands to primitive"
            (map unparse rand*) op))]
    [(void)
     (or (and (null? rand*)
              (case ctxt
                [(p) (make-constant #t)]
                [else (make-constant (void))]))
         (giveup))]
    [(car cdr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (constant-value a
                  (lambda (v)
                    (and (pair? v)
                         (mk-seq a
                           (make-constant
                             (case op
                               [(car) (car v)]
                               [else  (cdr v)]))))))))
         (giveup))]
    [(cadr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (and (pair? v)
                             (pair? (cdr v))
                             (mk-seq a
                               (make-constant
                                 (cadr v))))))
                    (make-funcall (make-primref op) rand*))))
         (giveup))] 
    [(not null? pair? fixnum? vector? string? char? symbol?
      eof-object?)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (case ctxt
                  [(e) a]
                  [else
                   (or (constant-value a
                         (lambda (v)
                           (mk-seq a
                             (make-constant
                               (case op
                                 [(not) (not v)]
                                 [(null?) (null? v)]
                                 [(pair?) (pair? v)]
                                 [(fixnum?) (fixnum? v)]
                                 [(vector?) (vector? v)]
                                 [(string?) (string? v)]
                                 [(char?) (char? v)]
                                 [(symbol?) (symbol? v)]
                                 [(eof-object?) (eof-object? v)]
                                 [else 
                                  (error 'optimize
                                    "huh ~s" op)])))))
                       (make-funcall (make-primref op) rand*))])))
         (giveup))]
    [($car $cdr)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (if (pair? v)
                            (make-seq a
                              (make-constant
                                (case op
                                  [($car) (car v)]
                                  [else   (cdr v)])))
                            (error 'optimize
                                   "incorrect arg ~s to ~s"
                                   v op))))
                    (giveup))))
         (error 'optimize "incorrect args to primitive"
                (map unparse rand*) op))]
    [(fxadd1 fxsub1)
     (or (and (fx= (length rand*) 1)
              (let ([a (car rand*)])
                (or (constant-value a
                      (lambda (v)
                        (and (fixnum? v)
                             (let ([t 
                                    (case op
                                      [(fxadd1) (add1 v)]
                                      [else     (sub1 v)])])
                                (and (fixnum? t)
                                     (mk-seq a 
                                       (make-constant t)))))))
                    (make-funcall (make-primref op) rand*))))
         (giveup))]
    [(fx+)
     (or (and (fx= (length rand*) 2)
              (let ([a0 (car rand*)] [a1 (cadr rand*)])
                (or (constant-value a1
                      (lambda (v1)
                        (and (fixnum? v1)
                             (or (constant-value a0
                                   (lambda (v0)
                                     (and (fixnum? v0)
                                          (let ([r (+ v0 v1)])
                                            (and (fixnum? r)
                                                 (mk-seq (mk-seq a0 a1) 
                                                   (make-constant r)))))))
                                 (mk-seq a1 
                                   (make-funcall (make-primref op) 
                                     (list a0 (make-constant v1))))))))
                    (constant-value a0
                      (lambda (v0)
                        (and (fixnum? v0)
                             (mk-seq a0 
                               (make-funcall (make-primref op)
                                  (list (make-constant v0) a1))))))
                    (make-funcall (make-primref op) rand*))))
         (giveup))]
    [(-)
     (or (and (>= (length rand*) 1)
              (andmap 
                (lambda (x) 
                  (constant-value x number?))
                rand*)
              (begin 
                (let ([r (apply - 
                           (map (lambda (x)
                                  (constant-value x 
                                    (lambda (v) v)))
                                rand*))])
                  (let f ([rand* rand*])
                    (cond
                      [(null? rand*) (make-constant r)]
                      [else
                       (mk-seq (car rand*) (f (cdr rand*)))])))))
         (giveup))]
    [(+ *)
     (or (and (>= (length rand*) 0)
              (andmap 
                (lambda (x) 
                  (constant-value x number?))
                rand*)
              (begin 
                (let ([r (apply 
                           (case op
                             [(+) +]
                             [(*) *]
                             [else (error 'ikarus "BUG: no prim" op)])
                           (map (lambda (x)
                                  (constant-value x 
                                    (lambda (v) v)))
                                rand*))])
                  (let f ([rand* rand*])
                    (cond
                      [(null? rand*) (make-constant r)]
                      [else
                       (mk-seq (car rand*) (f (cdr rand*)))])))))
         (giveup))]
    [(expt)
     (or (and (= (length rand*) 2)
              (andmap 
                (lambda (x) 
                  (constant-value x 
                    (lambda (v) (or (fixnum? v) (bignum? v)))))
                rand*)
              (begin 
                (let ([r (apply expt 
                           (map (lambda (x)
                                  (constant-value x 
                                    (lambda (v) v)))
                                rand*))])
                  (let f ([rand* rand*])
                    (cond
                      [(null? rand*) (make-constant r)]
                      [else
                       (mk-seq (car rand*) (f (cdr rand*)))])))))
         (giveup))]
    ;X; [(fx- fx+ fx*)
    ;X;  (or (and (fx= (length rand*) 2)
    ;X;           (let ([a0 (car rand*)] [a1 (cadr rand*)])
    ;X;             (or (constant-value a1
    ;X;                   (lambda (v1)
    ;X;                     (and (fixnum? v1)
    ;X;                          (or (constant-value a0
    ;X;                                (lambda (v0)
    ;X;                                  (and (fixnum? v0)
    ;X;                                       (let ([r (case op
    ;X;                                                  [(fx+) (+ v0 v1)]
    ;X;                                                  [(fx-) (- v0 v1)]
    ;X;                                                  [(fx*) (* v0 v1)]
    ;X;                                                  [else (error 'compile "BOO")])])
    ;X;                                         (and (fixnum? r)
    ;X;                                               (mk-seq (mk-seq a0 a1)
    ;X;                                                 (make-constant r)))))))
    ;X;                              (mk-seq a1 (make-primcall op (list a0 v1)))))))
    ;X;                 (constant-value a0
    ;X;                   (lambda (v0)
    ;X;                     (and (fixnum? v0)
    ;X;                          (mk-seq a0 (make-primcall op (list v0 a1))))))
    ;X;                 (make-primcall op (list a0 a1)))))
    ;X;      (giveup))]
    ;;; unoptimizables
    [(error syntax-error $syntax-dispatch $sc-put-cte 
      apply) 
     (giveup)]
    ))


(define (mk-mvcall p c)
  (struct-case p
    [(funcall) (make-mvcall p c)]
    [(seq e0 e1)
     (make-seq e0 (mk-mvcall e1 c))]
    [(bind lhs* rhs* body)
     (make-bind lhs* rhs* (mk-mvcall body c))]
    [else (error 'mk-mvcall "invalid producer" (unparse p))]))


(define (copy-propagate x)
  (define who 'copy-propagate)
  (define the-void (make-constant (void)))
  (define (known-value x) 
    (struct-case x 
      [(constant) x] ; known
      [(primref)  x] ; known
      [(bind lhs* rhs* body) (known-value body)]
      [(fix lhs* rhs* body) (known-value body)]
      [(seq e0 e1) (known-value e1)]
      [else #f]))
    
  (define (same-values? x y)
    (cond
      [(constant? x)
       (and (constant? y) 
            (eq? (constant-value x) 
                 (constant-value y)))]
      [(primref? x)
       (and (primref? y)
            (eq? (primref-name x)
                 (primref-name y)))]
      [else #f]))
  (define (predicate-value x)
    (struct-case x
      [(constant t) (if t 't 'f)]
      [(bind lhs rhs body) (predicate-value body)]
      [(fix lhs rhs body) (predicate-value body)]
      [(seq e0 e1) (predicate-value e1)]
      [else #f]))
  (define (do-conditional e0 e1 e2 k)
    (let ([e0 (Pred e0)])
      (cond
        [(predicate-value e0) =>
         (lambda (v)
           (if (eq? v 't) (k e1) (k e2)))]
        [else
         (make-conditional e0 (k e1) (k e2))])))
  (define (partition-referenced lhs* rhs*)
    (cond
      [(null? lhs*) (values '() '() the-void)]
      [else
       (let ([lhs (car lhs*)] [rhs (car rhs*)])
         (let-values ([(lhs* rhs* eff*) 
                       (partition-referenced
                           (cdr lhs*) (cdr rhs*))])
           (cond
             [(var-referenced lhs)
              (values (cons lhs lhs*) (cons rhs rhs*) eff*)]
             [else
              (values lhs* rhs* 
                (mk-seq eff* 
                  (Effect rhs)))])))]))
  (define (partition/assign-known lhs* rhs*)
    (cond
      [(null? lhs*) (values '() '() the-void)]
      [else
       (let ([lhs (car lhs*)] [rhs (car rhs*)])
         (let-values ([(lhs* rhs* eff*) 
                       (partition/assign-known
                           (cdr lhs*) (cdr rhs*))])
           (cond
             [(and (not (var-assigned lhs)) (known-value rhs)) =>
              (lambda (v)
                (set-var-referenced! lhs v)
                (values lhs* rhs* (mk-seq eff* rhs)))]
             [else
              (values (cons lhs lhs*) (cons rhs rhs*) eff*)])))]))
  (define (do-bind lhs* rhs* body k) 
    (let-values ([(lhs* rhs* eff0)
                  (partition-referenced lhs* rhs*)])
      (let ([rhs* (map Value rhs*)])
        (let-values ([(lhs* rhs* eff1)
                      (partition/assign-known lhs* rhs*)])
          (let ([body
                 (cond
                   [(null? lhs*) (k body)]
                   [else
                    (make-bind lhs* rhs* (k body))])])
            (mk-seq (mk-seq eff0 eff1) body))))))
  (define (do-fix lhs* rhs* body k) 
    (let-values ([(lhs* rhs* eff*) 
                  (partition-referenced lhs* rhs*)])
      (cond
        [(null? lhs*) (k body)]
        [else
         (make-fix lhs* (map Value rhs*) (k body))])))
  (define (mk-seq e0 e1)  ;;; keep e1 seq-free.
    (cond
      [(and (primcall? e0) (eq? (primcall-op e0) 'void)) e1]
      [(primref? e0) e1]
      [(seq? e1)
       (make-seq (make-seq e0 (seq-e0 e1)) (seq-e1 e1))]
      [else
       (make-seq e0 e1)]))
  (define (do-clambda g cls* cp free name)
    (make-clambda g
      (map (lambda (cls)
             (struct-case cls
               [(clambda-case info body)
                (make-clambda-case info (Value body))]))
           cls*)
      cp free name))
  (define (MKEffect ctxt)
    (define (Effect x)
      (struct-case x
        [(constant) the-void]
        [(var)      the-void]
        [(primref)  the-void]
        [(bind lhs* rhs* body)
         (do-bind lhs* rhs* body Effect)]
        [(fix lhs* rhs* body)
         (do-fix lhs* rhs* body Effect)]
        [(conditional e0 e1 e2)
         (let ([e0 (Pred e0)])
           (cond
             [(predicate-value e0) =>
              (lambda (v)
                (mk-seq e0 (if (eq? v 't) (Effect e1) (Effect e2))))]
             [else
              (make-conditional e0 (Effect e1) (Effect e2))]))]
        [(seq e0 e1) (mk-seq (Effect e0) (Effect e1))]
        [(clambda g cls*) the-void]
        [(primcall rator rand*) 
         (optimize-primcall ctxt rator (map Value rand*))]
        [(funcall rator rand*)
         (let ([rator (Value rator)])
           (cond
             [(known-value rator) =>
              (lambda (v)
                (struct-case v
                  [(primref op)
                   (mk-seq rator
                      (optimize-primcall ctxt op (map Value rand*)))]
                  [else
                   (make-funcall rator (map Value rand*))]))]
             [else (make-funcall rator (map Value rand*))]))]
        [(forcall rator rand*) 
         (make-forcall rator (map Value rand*))]
        [(mvcall p c)
         (mk-mvcall (Value p) (Value c))]
        [(assign lhs rhs)
         (unless (var-assigned lhs)
           (error who "var is not assigned" lhs))
         (if (var-referenced lhs)
             (make-assign lhs (Value rhs))
             (Effect rhs))]
        [else (error who "invalid effect expression" (unparse x))]))
    Effect)
  (define Effect (MKEffect 'e))
  (define (Pred x)
    (struct-case x
      [(constant) x]
      [(var) 
       (let ([r (var-referenced x)])
         (cond
           [(boolean? r) x]
           [else (Pred r)]))]
      [(primref) (make-constant #t)]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body Pred)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* body Pred)]
      [(conditional e0 e1 e2)
       (let ([e0 (Pred e0)])
         (cond
           [(predicate-value e0) =>
            (lambda (t0)
              (mk-seq e0 (if (eq? t0 't) (Pred e1) (Pred e2))))]
           [else
            (let ([e1 (Pred e1)] [e2 (Pred e2)])
              (cond
                [(predicate-value e1) =>
                 (lambda (t1)
                   (cond
                     [(predicate-value e2) =>
                      (lambda (t2)
                        (if (eq? t1 t2)
                            (mk-seq (make-conditional e0 e1 e2) 
                                    (make-constant (if (eq? t1 't) #t #f)))
                            (make-conditional e0 e1 e2)))]
                     [else (make-conditional e0 e1 e2)]))]
                [else (make-conditional e0 e1 e2)]))]))]
      [(seq e0 e1) (mk-seq (Effect e0) (Pred e1))]
      [(clambda g cls*) (make-constant #t)]
      [(primcall rator rand*) 
       (optimize-primcall 'p rator (map Value rand*))]
      [(funcall rator rand*)
       (let ([rator (Value rator)])
         (cond
           [(known-value rator) =>
            (lambda (v)
              (struct-case v
                [(primref op)
                 (mk-seq rator
                    (optimize-primcall 'p op (map Value rand*)))]
                [else
                 (make-funcall rator (map Value rand*))]))]
           [else (make-funcall rator (map Value rand*))]))]
      [(forcall rator rand*) 
       (make-forcall rator (map Value rand*))]
      [(assign lhs rhs)
       (mk-seq (Effect x) (make-constant #t))]
      [(mvcall p c)
       (mk-mvcall (Value p) (Value c))]
      [else (error who "invalid pred expression" (unparse x))]))
  (define (Value x)
    (struct-case x
      [(constant) x]
      [(var) 
       (let ([r (var-referenced x)])
         (case r
           [(#t) x]
           [(#f) (error who "Reference to a var that should not be" x)]
           [else r]))]
      [(primref) x]
      [(bind lhs* rhs* body)
       (do-bind lhs* rhs* body Value)]
      [(fix lhs* rhs* body)
       (do-fix lhs* rhs* body Value)]
      [(conditional e0 e1 e2)
       (let ([e0 (Pred e0)])
         (cond
           [(predicate-value e0) =>
            (lambda (t0)
              (mk-seq e0 (if (eq? t0 't) (Value e1) (Value e2))))]
           [else
            (let ([e1 (Value e1)] [e2 (Value e2)])
              (let ([t1 (known-value e1)] [t2 (known-value e2)])
                (cond
                  [(and t1 t2) 
                   (if (same-values? t1 t2)
                       (mk-seq (make-conditional e0 e1 e2) t1)
                       (make-conditional e0 e1 e2))]
                  [else (make-conditional e0 e1 e2)])))]))]
      [(seq e0 e1) (mk-seq (Effect e0) (Value e1))]
      [(clambda g cls* cp free name) 
       (do-clambda g cls* cp free name)]
      [(primcall rator rand*) 
       (optimize-primcall 'v rator (map Value rand*))]
      [(funcall rator rand*)
       (let ([rator (Value rator)])
         (cond
           [(known-value rator) =>
            (lambda (v)
              (struct-case v
                [(primref op)
                 (mk-seq rator
                    (optimize-primcall 'v op (map Value rand*)))]
                [else
                 (make-funcall rator (map Value rand*))]))]
           [else (make-funcall rator (map Value rand*))]))]
      [(forcall rator rand*) 
       (make-forcall rator (map Value rand*))]
      [(assign lhs rhs)
       (mk-seq (Effect x) the-void)]
      [(mvcall p c)
       (mk-mvcall (Value p) (Value c))]
      [else (error who "invalid value expression" (unparse x))]))
  (let ([x (Value x)])
    ;;; since we messed up the references and assignments here, we
    ;;; redo them
    (uncover-assigned/referenced x)))


(define (rewrite-assignments x)
  (define who 'rewrite-assignments)
  (define (fix-lhs* lhs*)
    (cond
      [(null? lhs*) (values '() '() '())]
      [else
       (let ([x (car lhs*)])
         (let-values ([(lhs* a-lhs* a-rhs*) (fix-lhs* (cdr lhs*))])
           (cond
             [(and (var-assigned x) (not (var-global-loc x)))
              (let ([t (unique-var 'assignment-tmp)])
                (values (cons t lhs*) (cons x a-lhs*) (cons t a-rhs*)))]
             [else
              (values (cons x lhs*) a-lhs* a-rhs*)])))]))
  (define (bind-assigned lhs* rhs* body)
    (cond
      [(null? lhs*) body]
      [else
       (make-bind lhs*
         (map (lambda (rhs) (make-funcall (make-primref 'vector) (list rhs))) rhs*)
         body)]))
  (define (Expr x)
    (struct-case x
      [(constant) x]
      [(var) 
       (cond
         [(var-assigned x)
          (cond
            [(var-global-loc x) =>
             (lambda (loc) 
               (make-funcall 
                 (make-primref '$symbol-value)
                 (list (make-constant loc))))]
            [else
             (make-funcall (make-primref '$vector-ref)
               (list x (make-constant 0)))])]
         [else x])]
      [(primref) x]
      [(bind lhs* rhs* body)
       (let-values ([(lhs* a-lhs* a-rhs*) (fix-lhs* lhs*)]) 
         (make-bind lhs* (map Expr rhs*) 
           (bind-assigned a-lhs* a-rhs* (Expr body))))]
      [(fix lhs* rhs* body)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls* cp free name) 
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  [(clambda-case info body)
                   (struct-case info
                     [(case-info label fml* proper)
                      (let-values ([(fml* a-lhs* a-rhs*) (fix-lhs* fml*)])
                        (make-clambda-case 
                          (make-case-info label fml* proper)
                          (bind-assigned a-lhs* a-rhs* (Expr body))))])]))
              cls*)
         cp free name)]
      [(forcall op rand*)
       (make-forcall op (map Expr rand*))]
      [(funcall rator rand*)
       (make-funcall (Expr rator) (map Expr rand*))]
      [(assign lhs rhs)
       (unless (var-assigned lhs)
         (error 'rewrite-assignments "not assigned" lhs x))
       (cond
         [(var-global-loc lhs) =>
          (lambda (loc) 
            (make-funcall (make-primref '$set-symbol-value!)
              (list (make-constant loc) (Expr rhs))))]
         [else
          (make-funcall (make-primref '$vector-set!)
            (list lhs (make-constant 0) (Expr rhs)))])]
      [(mvcall p c) (make-mvcall (Expr p) (Expr c))]
      [else (error who "invalid expression" (unparse x))]))
  (Expr x))





(define (optimize-for-direct-jumps x)
  (define who 'optimize-for-direct-jumps)
  (define (init-var x)
    (set-var-referenced! x #f))
  (define (set-var x v)
    (struct-case v
      [(clambda) (set-var-referenced! x v)]
      [(var) 
       (cond
         [(bound-var v) => (lambda (v) (set-var-referenced! x v))]
         [else (void)])]
      [else (void)]))
  (define (bound-var x)
    (var-referenced x))
  (define (optimize c rator rand*)
    (let ([n (length rand*)])
      (struct-case c
        [(clambda main-label cls*)
         (let f ([cls* cls*])
           (cond
             [(null? cls*) 
              ;;; none matching?
              (make-funcall rator rand*)]
             [else
              (struct-case (clambda-case-info (car cls*))
                [(case-info label fml* proper)
                 (cond
                   [proper
                    (if (fx= n (length fml*))
                        (make-jmpcall label rator rand*)
                        (f (cdr cls*)))]
                   [else
                    (if (fx<= (length (cdr fml*)) n)
                        (make-jmpcall label rator
                           (let f ([fml* (cdr fml*)] [rand* rand*])
                             (cond
                               [(null? fml*) 
                                ;;; FIXME: construct list afterwards
                                (list (make-funcall (make-primref 'list) rand*))]
                               [else
                                (cons (car rand*)
                                      (f (cdr fml*) (cdr rand*)))])))
                        (f (cdr cls*)))])])]))])))
  (define (Expr x)
    (struct-case x
      [(constant) x]
      [(var)      x]
      [(primref)  x]
      [(bind lhs* rhs* body)
       (for-each init-var lhs*)
       (let ([rhs* (map Expr rhs*)])
         (for-each set-var lhs* rhs*)
         (make-bind lhs* rhs* (Expr body)))]
      [(fix lhs* rhs* body)
       (for-each set-var lhs* rhs*)
       (make-fix lhs* (map Expr rhs*) (Expr body))]
      [(conditional test conseq altern)
       (make-conditional (Expr test) (Expr conseq) (Expr altern))]
      [(seq e0 e1) (make-seq (Expr e0) (Expr e1))]
      [(clambda g cls* cp free name) 
       (make-clambda g
         (map (lambda (cls)
                (struct-case cls
                  [(clambda-case info body)
                   (for-each init-var (case-info-args info))
                   (make-clambda-case info (Expr body))]))
              cls*)
         cp free name)]
      [(forcall op rand*)
       (make-forcall op (map Expr rand*))]
      [(funcall rator rand*)
       (let ([rator (Expr rator)])
         (cond
           [(and (var? rator) (bound-var rator)) =>
            (lambda (c)
              (optimize c rator (map Expr rand*)))]
           [(and (primref? rator)
                 (eq? (primref-name rator) '$$apply))
            (make-jmpcall (sl-apply-label) 
                          (Expr (car rand*))
                          (map Expr (cdr rand*)))]
           [else
            (make-funcall rator (map Expr rand*))]))]
      [(mvcall p c) (make-mvcall (Expr p) (Expr c))]
      [else (error who "invalid expression" (unparse x))]))
  (Expr x))


 

(define (convert-closures prog)
  (define who 'convert-closures)
  (define (Expr* x*)
    (cond
      [(null? x*) (values '() '())]
      [else
       (let-values ([(a a-free) (Expr (car x*))]
                    [(d d-free) (Expr* (cdr x*))])
         (values (cons a d) (union a-free d-free)))]))
   (define (do-clambda* lhs* x*)
    (cond
      [(null? x*) (values '() '())]
      [else
       (let-values ([(a a-free) (do-clambda (car lhs*) (car x*))]
                    [(d d-free) (do-clambda* (cdr lhs*) (cdr x*))])
         (values (cons a d) (union a-free d-free)))]))
  (define (do-clambda lhs x)
    (struct-case x 
      [(clambda g cls* _cp _free name)
       (let-values ([(cls* free) 
                     (let f ([cls* cls*])
                       (cond
                         [(null? cls*) (values '() '())]
                         [else
                          (struct-case (car cls*)
                            [(clambda-case info body)
                             (let-values ([(body body-free) (Expr body)]
                                          [(cls* cls*-free) (f (cdr cls*))])
                               (values
                                 (cons (make-clambda-case info body) cls*)
                                 (union (difference body-free (case-info-args info))
                                        cls*-free)))])]))])
          (let ([free (difference free (list lhs))])
            (values 
              (make-closure 
                (make-clambda g cls* lhs free name)
                free)
              free)))]))
  (define (Expr ex)
    (struct-case ex
      [(constant) (values ex '())]
      [(var) (values ex (singleton ex))]
      [(primref) (values ex '())]
      [(bind lhs* rhs* body)
       (let-values ([(rhs* rhs-free) (Expr* rhs*)] 
                    [(body body-free) (Expr body)])
          (values (make-bind lhs* rhs* body)
                  (union rhs-free (difference body-free lhs*))))]
      [(fix lhs* rhs* body)
       (let-values ([(rhs* rfree) (do-clambda* lhs* rhs*)]
                    [(body bfree) (Expr body)])
          (values (make-fix lhs* rhs* body)
                  (difference (union bfree rfree) lhs*)))]
      [(conditional test conseq altern)
       (let-values ([(test test-free) (Expr test)]
                    [(conseq conseq-free) (Expr conseq)]
                    [(altern altern-free) (Expr altern)])
         (values (make-conditional test conseq altern)
                 (union test-free (union conseq-free altern-free))))]
      [(seq e0 e1) 
       (let-values ([(e0 e0-free) (Expr e0)]
                    [(e1 e1-free) (Expr e1)])
         (values (make-seq e0 e1) (union e0-free e1-free)))]
      [(clambda)
       (do-clambda #f ex)]
      [(forcall op rand*)
       (let-values ([(rand* rand*-free) (Expr* rand*)])
         (values (make-forcall op rand*)  rand*-free))]
      [(funcall rator rand*)
       (let-values ([(rator rat-free) (Expr rator)]
                    [(rand* rand*-free) (Expr* rand*)])
         (values (make-funcall rator rand*) 
                 (union rat-free rand*-free)))]
      [(jmpcall label rator rand*)
       (let-values ([(rator rat-free) (Expr rator)]
                    [(rand* rand*-free) (Expr* rand*)])
         (values (make-jmpcall label rator rand*) 
                 (union rat-free rand*-free)))] 
      [(mvcall p c)
       (let-values ([(p p-free) (Expr p)]
                    [(c c-free) (Expr c)])
         (struct-case c
           [(closure code free^) 
            (values (make-mvcall p code)
                    (union p-free c-free))]
           [else (error who "invalid mvcall consumer" 
                        (unparse c))]))]
      [else (error who "invalid expression" (unparse ex))]))
  (let-values ([(prog free) (Expr prog)])
    (unless (null? free) 
      (error 'convert-closures "free vars encountered in program"
          free (unparse prog)))
   prog))


 




(define (optimize-closures/lift-codes x)
  (define who 'optimize-closures/lift-codes)
  (define all-codes '())
  (define (init-non-thunk var)
    (set-var-assigned! var #f)
    (set-var-referenced! var #f))
  (define (var-thunk var)
    (var-referenced var))
  (define (make-thunk-var var thunk)
    (set-var-referenced! var thunk))
  (define (thunk? x)
    (struct-case x
      [(closure code free*)
       (null? free*)]
      [else #f]))
  (define (trim/lift-code code free*)
    (struct-case code
      [(clambda label cls* cp free*/dropped name)
       (let ([cls* (map
                     (lambda (x)
                       (struct-case x 
                         [(clambda-case info body)
                          (for-each init-non-thunk
                              (case-info-args info))
                          (make-clambda-case info (E body))]))
                     cls*)])
         (let ([g (make-code-loc label)])
           (set! all-codes
             (cons (make-clambda label cls* cp free* name) all-codes))
           g))]))
  (define (optimize-one-closure code free)
    (let ([free (trim-vars free)])
      (make-closure (trim/lift-code code free) free)))
  (define (trim p? ls)
    (cond
      [(null? ls) '()]
      [(p? (car ls)) (trim p? (cdr ls))]
      [else
       (cons (car ls) (trim p? (cdr ls)))]))
  (define (trim-vars ls)
    (trim var-thunk ls))
  (define (trim-thunks ls)
    (trim thunk? ls))
  (define (do-bind lhs* rhs* body)
    (for-each init-non-thunk lhs*)
    (let ([rhs* (map E rhs*)])
      (for-each (lambda (x v) 
                  (when (thunk? v) (make-thunk-var x v)))
                lhs* rhs*)
      (make-bind (trim-vars lhs*) (trim-thunks rhs*) (E body))))
  (define (do-fix lhs* rhs* body)
    (for-each init-non-thunk lhs*)
    (let ([free** ;;; trim the free lists first; after init.
           (map (lambda (x) (trim-vars (closure-free* x))) rhs*)])
      (define-struct node (name code deps whacked free))
      (let ([node* (map (lambda (lhs rhs) 
                          (let ([n (make-node lhs (closure-code rhs) '() #f '())])
                            (make-thunk-var lhs n)
                            n))
                        lhs* rhs*)])
        ;;; if x is free in y, then whenever x becomes a non-thunk,
        ;;; y also becomes a non-thunk.  Here, we mark these
        ;;; dependencies.
        (for-each 
          (lambda (my-node free*)
            (for-each (lambda (fvar)
                        (cond
                          [(var-thunk fvar) => ;;; one of ours
                           (lambda (her-node)
                             (set-node-deps! her-node 
                               (cons my-node (node-deps her-node))))]
                          [else ;;; not one of ours
                           (set-node-free! my-node
                             (cons fvar (node-free my-node)))]))
                      free*))
           node* free**)
        ;;; Next, we go over the list of nodes, and if we find one
        ;;; that has any free variables, we know it's a non-thunk,
        ;;; so we whack it and add it to all of its dependents.
        (let ()
          (define (process-node x)
            (unless (null? (node-free x))
              (unless (node-whacked x)
                (set-node-whacked! x #t)
                (for-each 
                  (lambda (y)
                    (set-node-free! y 
                      (cons (node-name x) (node-free y)))
                    (process-node y))
                  (node-deps x)))))
          (for-each process-node node*))
        ;;; Now those that have free variables are actual closures.
        ;;; Those with no free variables are actual thunks.
        (let ([rhs*
               (map
                 (lambda (node)
                   (let ([free (node-free node)])
                     (let ([closure
                            (make-closure (node-code node) free)])
                       (if (null? free)
                           (make-thunk-var (node-name node) closure)
                           (init-non-thunk (node-name node)))
                       closure)))
                 node*)])
          (for-each 
            (lambda (x)
              (set-closure-code! x
                (trim/lift-code 
                  (closure-code x)
                  (closure-free* x))))
            rhs*)
          ;;;
          (make-fix (trim-vars lhs*)
                    (trim-thunks rhs*)
                    (E body))))))
  (define (E x)
    (struct-case x
      [(constant) x]
      [(var)      (or (var-thunk x) x)]
      [(primref)  x]
      [(bind lhs* rhs* body) (do-bind lhs* rhs* body)]
      [(fix lhs* rhs* body) (do-fix lhs* rhs* body)]
      [(conditional test conseq altern)
       (make-conditional (E test) (E conseq) (E altern))]
      [(seq e0 e1)           (make-seq (E e0) (E e1))]
      [(closure c free)      (optimize-one-closure c free)]
      [(forcall op rand*)    (make-forcall op (map E rand*))]
      [(funcall rator rand*) (make-funcall (E rator) (map E rand*))]
      [(jmpcall label rator rand*) (make-jmpcall label (E rator) (map E rand*))]
      [(mvcall p c)
       (struct-case c 
         [(clambda label cases cp free name)
          (make-mvcall (E p) 
            (make-clambda label
              (map (lambda (x)
                     (struct-case x
                       [(clambda-case info body)
                        (make-clambda-case info (E body))]))
                   cases)
              cp free name))])]
      [else (error who "invalid expression" (unparse x))]))
  ;(when (assembler-output) 
  ;  (printf "BEFORE\n")
  ;  (pretty-print (unparse x)))
  (let ([x (E x)])
    (let ([v (make-codes all-codes x)])
      ;(when (assembler-output) 
      ;  (printf "AFTER\n")
      ;  (pretty-print (unparse v)))
      v)))




(define (insert-engine-checks x)
  (define (Tail x)
    (make-seq
      (make-interrupt-call 
        (make-primcall '$engine-check '())
        (make-funcall (make-primref '$do-event) '()))
      x))
  (define (CaseExpr x)
    (struct-case x 
      [(clambda-case info body)
       (make-clambda-case info (Tail body))]))
  (define (CodeExpr x)
    (struct-case x
      [(clambda L cases cp free name)
       (make-clambda L (map CaseExpr cases) cp free name)]))
  (define (CodesExpr x)
    (struct-case x 
      [(codes list body)
       (make-codes (map CodeExpr list) (Tail body))]))
  (CodesExpr x))


(begin ;;; DEFINITIONS
  (module (wordsize)
    (include "ikarus.config.ss"))
  (define wordshift
    (case wordsize
      [(4) 2]
      [(8) 3]
      [else 
       (error 'ikarus "wordsize is neither 4 nor 8" wordsize)]))
  (define fx-scale wordsize)
  (define object-alignment (* 2 wordsize))
  (define align-shift (+ wordshift 1))
  (define fx-shift  wordshift)
  (define fx-mask   (- wordsize 1))
  (define fx-tag    0)
  (define bool-f #x2F)
  (define bool-t #x3F)
  (define bool-mask #xEF)
  (define bool-tag #x2F)
  (define bool-shift 4)
  (define nil     #x4F)
  (define eof     #x5F) ; double check
  (define unbound #x6F) ; double check
  (define void-object #x7F) ; double check
  (define bwp-object  #x8F) ; double check
  (define char-shift 8)
  (define char-tag #x0F)
  (define char-mask #xFF)
  (define pair-mask 7)
  (define pair-tag  1)
  (define disp-car  0)
  (define disp-cdr  wordsize)
  (define pair-size (* 2 wordsize))

  (define flonum-tag    #x17)
  (define flonum-size     16)
  (define disp-flonum-data 8)

  (define ratnum-tag    #x27)
  (define disp-ratnum-num  (* 1 wordsize))
  (define disp-ratnum-den  (* 2 wordsize))
  (define ratnum-size      (* 4 wordsize))

  (define bignum-mask        #b111)
  (define bignum-tag         #b011)
  (define bignum-sign-mask  #b1000)
  (define bignum-sign-shift   3)
  (define bignum-length-shift 4) 
  (define disp-bignum-data    wordsize)

  (define pagesize 4096)
  (define pageshift 12)
  
  (define bytevector-mask 7)
  (define bytevector-tag 2)
  (define disp-bytevector-length 0)
  (define disp-bytevector-data   8)

  (define ptag-mask 7)
  (define symbol-ptag 5)
  (define symbol-record-tag #x5F)
  (define disp-symbol-record-string  (* 1 wordsize))
  (define disp-symbol-record-ustring (* 2 wordsize))
  (define disp-symbol-record-value   (* 3 wordsize))
  (define disp-symbol-record-proc    (* 4 wordsize))
  (define disp-symbol-record-plist   (* 5 wordsize))
  (define symbol-record-size         (* 6 wordsize))
  
  (define record-tag  5)
  (define record-mask 7)

  (define vector-tag 5)
  (define vector-mask 7)
  (define disp-vector-length          0)
  (define disp-vector-data            wordsize)
  (define string-mask 7)
  (define string-tag 6)
  (define disp-string-length          0)
  (define disp-string-data            wordsize)
  (define closure-mask 7)
  (define closure-tag 3)
  (define disp-closure-code           0)
  (define disp-closure-data           wordsize)
  (define continuation-tag      #x1F)
  (define disp-continuation-top       (* 1 wordsize))
  (define disp-continuation-size      (* 2 wordsize))
  (define disp-continuation-next      (* 3 wordsize))
  (define continuation-size           (* 4 wordsize))
  (define code-tag              #x2F)
  (define disp-code-instrsize         (* 1 wordsize))
  (define disp-code-relocsize         (* 2 wordsize))
  (define disp-code-freevars          (* 3 wordsize))
  (define disp-code-annotation        (* 4 wordsize))
  (define disp-code-unused            (* 5 wordsize))
  (define disp-code-data              (* 6 wordsize))
  
  (define transcoder-mask                  #xFF) ;;; 0011
  (define transcoder-tag                   #x7F) ;;; 0011
  (define transcoder-payload-shift           10)

  (define transcoder-write-utf8-mask     #x1000) 
  (define transcoder-write-byte-mask     #x2000) 
  (define transcoder-read-utf8-mask      #x4000) 
  (define transcoder-read-byte-mask      #x8000) 
  (define transcoder-handling-mode-shift     16)
  (define transcoder-handling-mode-bits       2)
  (define transcoder-eol-style-shift         18)
  (define transcoder-eol-style-bits           3)
  (define transcoder-codec-shift             21)
  (define transcoder-codec-bits               3)
  
  (define transcoder-handling-mode:none    #b00)
  (define transcoder-handling-mode:ignore  #b01)
  (define transcoder-handling-mode:raise   #b10)
  (define transcoder-handling-mode:replace #b11)

  (define transcoder-eol-style:none       #b000)
  (define transcoder-eol-style:lf         #b001)
  (define transcoder-eol-style:cr         #b010)
  (define transcoder-eol-style:crlf       #b011)
  (define transcoder-eol-style:nel        #b100)
  (define transcoder-eol-style:crnel      #b101)
  (define transcoder-eol-style:ls         #b110)
  
  (define transcoder-codec:none           #b000)
  (define transcoder-codec:latin-1        #b001)
  (define transcoder-codec:utf-8          #b010)
  (define transcoder-codec:utf-16         #b011)

  (define port-tag               #x3F)
  (define port-mask              #x3F)
  (define disp-port-attrs           0)
  (define disp-port-index           (* 1 wordsize))
  (define disp-port-size            (* 2 wordsize))
  (define disp-port-buffer          (* 3 wordsize))
  (define disp-port-transcoder      (* 4 wordsize))
  (define disp-port-id              (* 5 wordsize))
  (define disp-port-read!           (* 6 wordsize))
  (define disp-port-write!          (* 7 wordsize))
  (define disp-port-get-position    (* 8 wordsize))
  (define disp-port-set-position!   (* 9 wordsize))
  (define disp-port-close           (* 10 wordsize))
  (define disp-port-cookie          (* 11 wordsize))
  (define disp-port-position        (* 12 wordsize))
  (define disp-port-unused          (* 13 wordsize))
  (define port-size                 (* 14 wordsize))

  (define disp-tcbucket-tconc 0)
  (define disp-tcbucket-key         (* 1 wordsize))
  (define disp-tcbucket-val         (* 2 wordsize))
  (define disp-tcbucket-next        (* 3 wordsize))
  (define tcbucket-size             (* 4 wordsize))
  (define record-ptag  5)
  (define record-pmask 7)
  (define disp-struct-rtd     0)
  (define disp-struct-data    wordsize)

  ;;; refer to the picture in src/ikarus-collect.c for details
  ;;; on how call-frames are laid out.  (search for livemask)
  (define call-instruction-size 
    (case wordsize
      [(4) 5]
      [(8) 10]
      [else (die 'call-instruction-size "invalid" wordsize)]))
  (define disp-frame-size    (- (+ call-instruction-size (* 3 wordsize))))
  (define disp-frame-offset  (- (+ call-instruction-size (* 2 wordsize))))
  (define disp-multivalue-rp (- (+ call-instruction-size (* 1 wordsize))))

  (define dirty-word -1))

;(define pcb-allocation-pointer    (*  0 wordsize)) NOT USED
(define pcb-allocation-redline     (* 1 wordsize))
;(define pcb-frame-pointer         (*  2 wordsize)) NOT USED
(define pcb-frame-base             (* 3 wordsize))
(define pcb-frame-redline          (* 4 wordsize))
(define pcb-next-continuation      (* 5 wordsize))
;(define pcb-system-stack          (*  6 wordsize)) NOT USED
(define pcb-dirty-vector           (* 7 wordsize))
(define pcb-arg-list               (* 8 wordsize))
(define pcb-engine-counter         (* 9 wordsize))
(define pcb-interrupted            (* 10 wordsize))
(define pcb-base-rtd               (* 11 wordsize))
(define pcb-collect-key            (* 12 wordsize))


(define (fx? x)
  (let* ([intbits (* wordsize 8)]
         [fxbits (- intbits fx-shift)])
    (and (or (fixnum? x) (bignum? x))
         (<= (- (expt 2 (- fxbits 1)))
             x
             (- (expt 2 (- fxbits 1)) 1)))))


(module ()
  ;;; initialize the cogen
  (code-entry-adjustment (- disp-code-data vector-tag)))

(begin ;;; COGEN HELERS
  (define (align n)
    (fxsll (fxsra (fx+ n (fxsub1 object-alignment)) align-shift) align-shift))
  (define (mem off val)
    (cond
      [(fixnum? off) (list 'disp (int off) val)]
      [(register? off) (list 'disp off val)]
      [else (error 'mem "invalid disp" off)]))
  (define-syntax int
    (syntax-rules ()
      [(_ x) x]))
  (define (obj x) (list 'obj x))
  (define (byte x) (list 'byte x))
  (define (byte-vector x) (list 'byte-vector x))
  (define (movzbl src targ) (list 'movzbl src targ))
  (define (sall src targ) (list 'sall src targ))
  (define (sarl src targ) (list 'sarl src targ))
  (define (shrl src targ) (list 'shrl src targ))
  (define (notl src) (list 'notl src))
  (define (pushl src) (list 'pushl src))
  (define (popl src) (list 'popl src))
  (define (orl src targ) (list 'orl src targ))
  (define (xorl src targ) (list 'xorl src targ))
  (define (andl src targ) (list 'andl src targ))
  (define (movl src targ) (list 'movl src targ))
  (define (leal src targ) (list 'leal src targ))
  (define (movb src targ) (list 'movb src targ))
  (define (addl src targ) (list 'addl src targ))
  (define (imull src targ) (list 'imull src targ))
  (define (idivl src) (list 'idivl src))
  (define (subl src targ) (list 'subl src targ))
  (define (push src) (list 'push src))
  (define (pop targ) (list 'pop targ))
  (define (sete targ) (list 'sete targ))
  (define (call targ) (list 'call targ))
  (define (tail-indirect-cpr-call)
    (jmp (mem (fx- disp-closure-code closure-tag) cpr)))
  (define (indirect-cpr-call)
    (call (mem (fx- disp-closure-code closure-tag) cpr)))
  (define (negl targ) (list 'negl targ))
  (define (label x) (list 'label x))
  (define (label-address x) (list 'label-address x))
  (define (ret) '(ret))
  (define (cltd) '(cltd))
  (define (cmpl arg1 arg2) (list 'cmpl arg1 arg2))
  (define (je label) (list 'je label))
  (define (jne label) (list 'jne label))
  (define (jle label) (list 'jle label))
  (define (jge label) (list 'jge label))
  (define (jg label) (list 'jg label))
  (define (jl label) (list 'jl label))
  (define (jb label) (list 'jb label))
  (define (ja label) (list 'ja label))
  (define (jo label) (list 'jo label))
  (define (jmp label) (list 'jmp label))
  (define esp '%esp) ; stack base pointer 
  (define al '%al)
  (define ah '%ah)
  (define bh '%bh)
  (define cl '%cl)
  (define eax '%eax)
  (define ebx '%ebx)
  (define ecx '%ecx)
  (define edx '%edx)
  (define apr '%ebp) ; allocation pointer
  (define fpr '%esp) ; frame pointer
  (define cpr '%edi) ; closure pointer
  (define pcr '%esi) ; pcb pointer
  (define register? symbol?)
  (define (argc-convention n)
    (fx- 0 (fxsll n fx-shift))))


(define (primref->symbol op)
  (unless (symbol? op) (error 'primref->symbol "not a symbol" op))
  (cond
    [((current-primitive-locations) op) =>
     (lambda (x)
       (unless (symbol? x) 
         (error 'primitive-location 
            "not a valid location for ~s" x op))
       x)]
    [else
     (let ()
       (define-condition-type &url &condition
         make-url-condition
         url-condition?
        (url condition-url))
       (raise 
         (condition 
           (make-error)
           (make-who-condition 'ikarus)
           (make-message-condition "primitive not supported yet")
           (make-message-condition
             "please file a bug report to help us prioritize our goals")
           (make-url-condition 
             "https://bugs.launchpad.net/ikarus/+filebug")
           (make-irritants-condition (list op)))))]))

(define (primref-loc op)
  (mem (fx- disp-symbol-record-proc record-tag) 
       (obj (primref->symbol op))))



(module ;assembly-labels
  (refresh-cached-labels!
   sl-apply-label sl-fx+-type-label sl-fx+-types-label
   sl-continuation-code-label sl-invalid-args-label
   sl-mv-ignore-rp-label sl-mv-error-rp-label sl-values-label 
   sl-cwv-label sl-top-level-value-error-label sl-cadr-error-label
   sl-cdr-error-label sl-car-error-label sl-nonprocedure-error-label
   sl-fxsub1-error-label sl-fxadd1-error-label sl-fx+-overflow-label)
  (define-syntax define-cached
    (lambda (x)
      (syntax-case x ()
        [(_ refresh [(name*) b* b** ...] ...)
         (with-syntax ([(v* ...) (generate-temporaries #'(name* ...))])
           #'(begin
               (define v* #f) ...
               (define (name*)
                 (or v* (error 'name* "uninitialized label"))) ...
               (define (refresh)
                 (define-syntax name* 
                   (lambda (stx)
                     (syntax-error stx 
                        "cannot use label before it is defined")))
                 ...
                 (let* ([name* (let ([label (let () b* b** ...)])
                                 (set! v* label)
                                 (lambda () label))] ...)
                   (void)))))])))
  (define-cached refresh-cached-labels!
   [(sl-apply-label)
    (let ([SL_apply (gensym "SL_apply")]
          [L_apply_done (gensym)]
          [L_apply_loop (gensym)])
      (assemble-sources (lambda (x) #f)
        (list
          (list 0 
              (label SL_apply)
              (movl (mem fpr eax) ebx)
              (cmpl (int nil) ebx)
              (je (label L_apply_done))
              (label L_apply_loop)
              (movl (mem (fx- disp-car pair-tag) ebx) ecx)
              (movl (mem (fx- disp-cdr pair-tag) ebx) ebx)
              (movl ecx (mem fpr eax))
              (subl (int wordsize) eax)
              (cmpl (int nil) ebx)
              (jne (label L_apply_loop))
              (label L_apply_done)
              (addl (int wordsize) eax)
              (tail-indirect-cpr-call))))
      SL_apply)]
   [(sl-fx+-type-label)
    (define SL_fx+_type (gensym "SL_fx+_type"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
              (label SL_fx+_type)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (primref-loc 'fx+-type-error) cpr)
              (movl (int (argc-convention 1)) eax)
              (tail-indirect-cpr-call))))
    SL_fx+_type]
   [(sl-fx+-types-label)
    (define SL_fx+_types (gensym "SL_fx+_types"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fx+_types)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl ebx (mem (fx- wordsize wordsize) fpr))
          (movl (primref-loc 'fx+-types-error) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_fx+_types]
   [(sl-continuation-code-label)
    (define SL_continuation_code (gensym "SL_continuation_code"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ([L_cont_zero_args      (gensym)] 
              [L_cont_mult_args      (gensym)]
              [L_cont_one_arg        (gensym)]
              [L_cont_mult_move_args (gensym)]
              [L_cont_mult_copy_loop (gensym)])
          (list  1 ; freevars
              (label SL_continuation_code)
              (movl (mem (fx- disp-closure-data closure-tag) cpr) ebx) ; captured-k
              (movl ebx (mem pcb-next-continuation pcr)) ; set
              (movl (mem pcb-frame-base pcr) ebx)
              (cmpl (int (argc-convention 1)) eax)
              (jg (label L_cont_zero_args))
              (jl (label L_cont_mult_args))
              (label L_cont_one_arg)
              (movl (mem (fx- 0 wordsize) fpr) eax)
              (movl ebx fpr)
              (subl (int wordsize) fpr)
              (ret)
              (label L_cont_zero_args)
              (subl (int wordsize) ebx)
              (movl ebx fpr)
              (movl (mem 0 ebx) ebx) ; return point
              (jmp (mem disp-multivalue-rp ebx))  ; go
              (label L_cont_mult_args)
              (subl (int wordsize) ebx)
              (cmpl ebx fpr)
              (jne (label L_cont_mult_move_args))
              (movl (mem 0 ebx) ebx)
              (jmp (mem disp-multivalue-rp ebx))
              (label L_cont_mult_move_args)
              ; move args from fpr to ebx
              (movl (int 0) ecx)
              (label L_cont_mult_copy_loop)
              (subl (int wordsize) ecx)
              (movl (mem fpr ecx) edx)
              (movl edx (mem ebx ecx))
              (cmpl ecx eax)
              (jne (label L_cont_mult_copy_loop))
              (movl ebx fpr)
              (movl (mem 0 ebx) ebx)
              (jmp (mem disp-multivalue-rp ebx))))))
    SL_continuation_code]
   [(sl-invalid-args-label)
    (define SL_invalid_args (gensym "SL_invalid_args"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_invalid_args)
          ;;;
          (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
          (negl eax)
          (movl eax (mem (fx- 0 (fx* 2 wordsize)) fpr))
          (movl (primref-loc '$incorrect-args-error-handler) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_invalid_args]
   [(sl-mv-ignore-rp-label)
    (define SL_multiple_values_ignore_rp (gensym "SL_multiple_ignore_error_rp"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
           (label SL_multiple_values_ignore_rp)
           (ret))))
    SL_multiple_values_ignore_rp]
   [(sl-mv-error-rp-label)
    (define SL_multiple_values_error_rp (gensym "SL_multiple_values_error_rp"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_multiple_values_error_rp)
          (movl (primref-loc '$multiple-values-error) cpr)
          (tail-indirect-cpr-call))))
    SL_multiple_values_error_rp]
   [(sl-values-label)
    (define SL_values (gensym "SL_values"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ([L_values_one_value (gensym)]
              [L_values_many_values (gensym)])
          (list 0 ; no freevars
              '(name values)
              (label SL_values)
              (cmpl (int (argc-convention 1)) eax)
              (je (label L_values_one_value))
              (label L_values_many_values)
              (movl (mem 0 fpr) ebx) ; return point
              (jmp (mem disp-multivalue-rp ebx))     ; go
              (label L_values_one_value)
              (movl (mem (fx- 0 wordsize) fpr) eax)
              (ret)))))
    SL_values]
   [(sl-nonprocedure-error-label)
    (define SL_nonprocedure (gensym "SL_nonprocedure"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_nonprocedure)
          (movl cpr (mem (fx- 0 wordsize) fpr)) ; first arg
          (movl (primref-loc '$apply-nonprocedure-error-handler) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_nonprocedure]
   [(sl-cwv-label)
    (define SL_call_with_values (gensym "SL_call_with_values"))
    (assemble-sources (lambda (x) #f)
      (list
        (let ([L_cwv_done (gensym)]
              [L_cwv_loop (gensym)]
              [L_cwv_multi_rp (gensym)]
              [L_cwv_call (gensym)])
          (list 
              0 ; no free vars
              '(name call-with-values)
              (label SL_call_with_values)
              (cmpl (int (argc-convention 2)) eax)
              (jne (label (sl-invalid-args-label)))
              (movl (mem (fx- 0 wordsize) fpr) ebx) ; producer
              (movl ebx cpr)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label (sl-nonprocedure-error-label)))
              (movl (int (argc-convention 0)) eax)
              ;(subl (int (fx* wordsize 2)) fpr)
              (compile-call-frame
                 3
                 '#(#b110)
                 (label-address L_cwv_multi_rp)
                 (indirect-cpr-call))
              ;;; one value returned
              ;(addl (int (fx* wordsize 2)) fpr)
              (movl (mem (fx* -2 wordsize) fpr) ebx) ; consumer
              (movl ebx cpr)
              (movl eax (mem (fx- 0 wordsize) fpr))
              (movl (int (argc-convention 1)) eax)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label (sl-nonprocedure-error-label)))
              (tail-indirect-cpr-call)
              ;;; multiple values returned
              (label L_cwv_multi_rp)
              ; because values does not pop the return point
              ; we have to adjust fp one more word here
              (addl (int (fx* wordsize 3)) fpr) 
              (movl (mem (fx* -2 wordsize) fpr) cpr) ; consumer
              (cmpl (int (argc-convention 0)) eax)
              (je (label L_cwv_done))
              (movl (int (fx* -4 wordsize)) ebx)
              (addl fpr ebx)  ; ebx points to first value
              (movl ebx ecx)
              (addl eax ecx)  ; ecx points to the last value
              (label L_cwv_loop)
              (movl (mem 0 ebx) edx)
              (movl edx (mem (fx* 3 wordsize) ebx))
              (subl (int wordsize) ebx)
              (cmpl ecx ebx)
              (jge (label L_cwv_loop))
              (label L_cwv_done)
              (movl cpr ebx)
              (andl (int closure-mask) ebx)
              (cmpl (int closure-tag) ebx)
              (jne (label (sl-nonprocedure-error-label)))
              (tail-indirect-cpr-call)))))
    SL_call_with_values]
   [(sl-top-level-value-error-label)
    (define SL_top_level_value_error (gensym "SL_top_level_value_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_top_level_value_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'top-level-value-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_top_level_value_error]
   [(sl-cadr-error-label)
    (define SL_cadr_error (gensym "SL_cadr_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_cadr_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'cadr-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_cadr_error]
   [(sl-cdr-error-label)
    (define SL_cdr_error (gensym "SL_cdr_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_cdr_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'cdr-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_cdr_error]
   [(sl-car-error-label)
    (define SL_car_error (gensym "SL_car_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_car_error)
          (movl ebx (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'car-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_car_error]
   [(sl-fxsub1-error-label)
    (define SL_fxsub1_error (gensym "SL_fxsub1_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fxsub1_error)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'fxsub1-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_fxsub1_error]
   [(sl-fxadd1-error-label)
    (define SL_fxadd1_error (gensym "SL_fxadd1_error"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fxadd1_error)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl (primref-loc 'fxadd1-error) cpr)
          (movl (int (argc-convention 1)) eax)
          (tail-indirect-cpr-call))))
    SL_fxadd1_error]
   [(sl-fx+-overflow-label)
    (define SL_fx+_overflow (gensym "SL_fx+_overflow"))
    (assemble-sources (lambda (x) #f)
      (list
        (list 0
          (label SL_fx+_overflow)
          (movl eax (mem (fx- 0 wordsize) fpr))
          (movl ebx (mem (fx- wordsize wordsize) fpr))
          (movl (primref-loc 'fx+-overflow-error) cpr)
          (movl (int (argc-convention 2)) eax)
          (tail-indirect-cpr-call))))
    SL_fx+_overflow]))

(define (print-instr x)
  (cond
    [(and (pair? x) (eq? (car x) 'seq))
     (for-each print-instr (cdr x))]
    [else 
     (printf "    ~s\n" x)]))

(define (compile-core-expr->code p)
  (let* ([p (recordize p)]
         [p (parameterize ([open-mvcalls #f])
              (optimize-direct-calls p))]
         [p (optimize-letrec p)]
         [p (uncover-assigned/referenced p)]
         [p (copy-propagate p)]
         [p (rewrite-assignments p)]
         [p (optimize-for-direct-jumps p)]
         [p (convert-closures p)]
         [p (optimize-closures/lift-codes p)])
    (let ([ls* (alt-cogen p)])
      (when (assembler-output)
        (parameterize ([gensym-prefix "L"]
                       [print-gensym #f])
          (for-each 
            (lambda (ls)
              (newline)
              (for-each print-instr ls))
            ls*)))
      (let ([code* 
             (assemble-sources 
               (lambda (x)
                 (if (closure? x)
                     (if (null? (closure-free* x))
                         (code-loc-label (closure-code x))
                         (error 'compile "BUG: non-thunk escaped" x))
                     #f))
               ls*)])
        (car code*)))))

(define compile-core-expr-to-port
  (lambda (expr port)
    (fasl-write (compile-core-expr->code expr) port)))


(define (compile-core-expr x)
  (let ([code (compile-core-expr->code x)])
    ($code->closure code)))

(define assembler-output (make-parameter #f))


(define eval-core
  (lambda (x) ((compile-core-expr x))))

(include "ikarus.compiler.altcogen.ss")

(define current-primitive-locations
  (let ([plocs (lambda (x) #f)])
    (case-lambda
      [() plocs]
      [(p)
       (if (procedure? p)
           (begin 
             (set! plocs p) 
             (refresh-cached-labels!))
           (error 'current-primitive-locations "not a procedure" p))])))

)


