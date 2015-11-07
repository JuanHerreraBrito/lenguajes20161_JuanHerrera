#lang plai


;2 MList
#|(define-type MList
  [MEmpty]
  [MCons (n RCFAES?) (l RCFAES?)])
|#
#|

|#

(define-type Binding
  [bind (name symbol?) (val RCFAES?)])

(define-type RCFAES
  [numS (n number?)]
  [boolS (b boolean?)]
  [MConsS (l RCFAES?) (r RCFAES?)]
  [MEmptyS]
  [withS (bindings (listof bind?))
         (body RCFAES?)]
  [with*S (bindings (listof bind?))
          (body RCFAES?)]
  [recS (bindings (listof bind?))
         (body RCFAES?)]
  [idS (name symbol?)]
  [ifS (cond RCFAES?) (then RCFAES?) (else RCFAES?)]
  [equalS (left RCFAES?) (right RCFAES?)]
  [funS (params (listof symbol?))
        (body RCFAES?)]
  [appS (fun RCFAES?)
        (args (listof RCFAES?))]
  [opS (f procedure?)
         (p RCFAES?)]
  [binopS (f procedure?)
         (l RCFAES?)
         (r RCFAES?)])

(define-type RCFAEL
  [num (n number?)]
  [bool (b boolean?)]
  [MEmpty]
  [MCons (p RCFAEL?) (l RCFAEL?)]
  [id (name symbol?)]
  [iif (c RCFAEL?) (t RCFAEL?) (e RCFAEL?)]
  [fun (params (listof symbol?))
       (body RCFAEL?)]
  [app (fun RCFAEL?)
       (args (listof RCFAEL?))]
  [op (f procedure?)
         (p RCFAEL?)]
  [binop (f procedure?)
         (l RCFAEL?)
         (r RCFAEL?)])

(define-type RCFAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [closureV (param (listof symbol?))
            (body RCFAEL?)
            (env Env?)]
  [MEmptyV]
  [MConsV (p RCFAEL-Value?)
            (l RCFAEL-Value?)])


(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value RCFAEL-Value?) 
        (env Env?)])

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) (lambda (x y) (and x y))]
    [(or) (lambda (x y) (or x y))]
    ))
  
(define (eligeUnaria s)
  (case s
    [(inc) (lambda (x) (+ x 1))]
    [(dec) (lambda (x) (- x 1))]
    [(zero?) (lambda (x) (zero? x))]
    [(num?) (lambda (x) (number? x))]
    [(neg) (lambda (x) (not x))]
    [(bool?) (lambda (x) (boolean? x))]
    [(first) #f] ;falta implementar en listas
    [(rest) #f] ;falta implementar en listas
    [(empty?) #f] ;falta implementar en listas
    [(list?) #f] ;falta implementar en listas
    ))
 
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> FAES
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(MCons) (MConsS (parse (cadr sexp)) (parse (caddr sexp)))]
       [(MEmpty) (MEmptyS)]
       [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       [(if) (ifS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(inc dec zero? num? neg bool? first rest empty? list?) (opS (eligeUnaria (car sexp)) (parse (cadr sexp)))]
       [(+ - / * < > <= >= and or) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
