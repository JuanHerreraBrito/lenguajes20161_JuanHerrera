#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [withS (list-bind body) (app (fun (map get-name list-bind) (desugar body))
                                (map desugar (map get-value list-bind)))
           ]
    [with*S (list-bind body) (app (fun (map get-name list-bind) (desugar body))
                                (map desugar (map get-value list-bind)))]
    [idS (s) (id s)]
    [funS (p b) (fun p (desugar b))]
    [appS (f e) (app (desugar f) (map desugar e))]
    [binopS (op l r) (binop op (desugar l) (desugar r))]))

(define (get-name bd)
  (type-case Binding bd
    [bind (n v) n]))

(define (get-value bd)
  (type-case Binding bd
    [bind (n v) v]))

(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (desugar (parse '(fun (x) x))) (fun '(x) (id 'x)))
(test (desugar (parse '(fun (x y z) x))) (fun '(x y z) (id 'x)))
(test (desugar (parse '((fun (x y z) x) 5 6))) (app (fun '(x y z) (id 'x)) (list (num 5) (num 6))))
(test (desugar (parse '{with* {{x {+ 5 5}} {y 2}} {* x y}})) (app (fun '(x y) (binop * (id 'x) (id 'y))) (list (binop + (num 5) (num 5)) (num 2)) ))
(test (desugar (parse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z}))  (app (fun '(x y z) (id 'z)) (list (num 3) (binop + (num 2) (id 'x)) (binop + (id 'x) (id 'y)))) )


(define (cparse sexp)
  (desugar (parse sexp)))

(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [id (v) (lookup v env)]
    [fun (ls b) (closureV ls b env)]
    [app (fx arg-l) 
         (local ([define fval (interp fx env)])
           (if (closureV? fval)
               (interp (closureV-body fval)
                       (join-arg (closureV-param fval) arg-l (closureV-env fval)))
               (error 'interp (string-append (~a fx) " expression is not a function"))))]
    [binop (f l r) (applyV f (interp l env) (interp r env))]))


(define (join-arg param-l arg-l env)
  (cond
    [(or (and (empty? param-l) (not (empty? arg-l))) (and (not (empty? param-l)) (empty? arg-l))) " number of values diferent from number of args"]
    [(and (empty? param-l) (empty? arg-l)) env]
    [else (join-arg (cdr param-l) (cdr arg-l) (aSub (car param-l) (interp (car arg-l) env) env))]))

(define (lookup v env)
  (cond
    [(mtSub? env) "Error, variable not found"]
    [(aSub? env) (if (equal? (aSub-name env) v) (aSub-value env) (lookup v (aSub-env env)))]))

(define (applyV f l r)
  (numV (f (numV-n l) (numV-n r))))

(define (rinterp expr)
  (interp expr (mtSub)))


(test (rinterp (cparse '3)) (numV 3))
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