#lang plai

(require "practica5-base.rkt")

(print-only-errors true)

#|
(define-type opV
  [inc (i procedure?) (n number?)]
  [dec (d procedure?) (n number?)]
  [Mzero? (z procedure?) (n number?)]
  [Mnum? (n procedure?) (m number?)]
  [neg (n procedure?) (m number?)]
  [bool? (b procedure?) (v bool?)]
  [Mfirst (f procedure?) (l MList?)]
  [Mrest (f procedure?) (l MList?)]
  [Mempty? (f procedure?) (l MList?)]
  [Mlist? (f procedure?) (l MList?)])
|#

(define (desugar expr)
  (type-case RCFAES expr
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [mListS (l) (mList l)]#| verificar |#
    [withS (list-bind body) (app (fun (map get-name list-bind) (desugar body))
                                (map desugar (map get-value list-bind)))]
    [with*S (list-bind body) (app (fun (list (get-name (car list-bind))) (if (empty? (cdr list-bind)) (desugar body) (desugar (with*S (cdr list-bind) body))))
                                (list (desugar (get-value (car list-bind)))))]
    [recS (list-bind body) #| verificar |# #f]
    [idS (s) (id s)]
    [ifS (c t e) (iif (desugar c) (desugar t) (desugar e))]
    [equalS (left right)#| verificar |# #f]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f) (map desugar e))]
    [opS (f p) (op f (desugar p))]
    [binopS (op l r) (binop op (desugar l) (desugar r))]))

(define (get-name bd)
  (type-case Binding bd
    [bind (n v) n]))

(define (get-value bd)
  (type-case Binding bd
    [bind (n v) v]))

(desugar (parse '{and #f #t}))
(desugar (parse '#t))
(desugar (parse '{if {and #t #f} {+ 5 6} {- 5 4}}))
;(test (desugar (parse '{and #f #t})) (binop (lambda (x y) (and x y)) (bool #f) (bool #t)))

(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (desugar (parse '(fun (x) x))) (fun '(x) (id 'x)))
(test (desugar (parse '(fun (x y z) x))) (fun '(x y z) (id 'x)))
(test (desugar (parse '((fun (x y z) x) 5 6))) (app (fun '(x y z) (id 'x)) (list (num 5) (num 6))))
(test (desugar (parse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (app (fun '(x) (app (fun '(y) (app (fun '(z) (id 'z)) (list (binop + (id 'x) (id 'y))))) (list (binop + (num 2) (id 'x))))) (list (num 3))))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (interp expr env)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [id (v) (lookup v env)]
    [iif (c t e) (if (boolV-b (interp c env)) (interp t env) (interp e env))]
    [fun (ls b) (closureV ls b env)]
    [app (fx arg-l) 
         (local ((define fval (interp fx env)))
           (if (closureV? fval)
               (interp (closureV-body fval)
                       (join-arg (closureV-param fval) arg-l (closureV-env fval) (closureV-env fval)))
               (error 'interp (string-append (~a fx) " expression is not a function"))))]
    [op (f p) (applyV (f (getV (interp p env))))]
    [binop (f l r) (applyV (f (getV (interp l env)) (getV (interp r env))))]
    [else #f]))

(define (join-arg param-l arg-l env envOriginal)
  (cond
    [(or (and (empty? param-l) (not (empty? arg-l))) (and (not (empty? param-l)) (empty? arg-l))) " number of values diferent from number of args"]
    [(and (empty? param-l) (empty? arg-l)) env]
    [else (join-arg (cdr param-l) (cdr arg-l) (aSub (car param-l) (interp (car arg-l) envOriginal) env) envOriginal)]))


(define (lookup v env)
  (cond
    [(mtSub? env) (error 'lookup (string-append (symbol->string v) " symbol is not in the env")) ]
    [(aSub? env) (if (equal? (aSub-name env) v) (aSub-value env) (lookup v (aSub-env env)))]))

(define (applyV v)
  (cond
    [(number? v) (numV v)]
    [(boolean? v) (boolV v)]
    [else (error 'applyV (string-append (symbol->string v) " no es una operacion binaria"))]))

(define (getV expr)
  (type-case RCFAEL-Value expr
  [numV (n) n]
  [boolV (b) b]
  [else (error 'getV (string-append (symbol->string expr) " los tipos no coinciden"))]))

(define (rinterp expr)
  (interp expr (mtSub)))


(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '#f)) (boolV #f))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{and #f #t})) (boolV #f))
(test (rinterp (cparse '{<= 3 4})) (boolV #t))
(test (rinterp (cparse '{if {or #f #t} {+ 3 4} {+ 3 3}} )) (numV 7))
(test (rinterp (cparse '{inc 3})) (numV 4))
(test (rinterp (cparse '{neg #t})) (boolV #f))
(test (rinterp (cparse '{inc 3})) (numV 4))
(test (rinterp (cparse '{bool? 10})) (boolV #f))
(test (rinterp (cparse '{zero? 10})) (boolV #f))
(test (rinterp (cparse '{zero? 0})) (boolV #t))

(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))
(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "x symbol is not in the env")




