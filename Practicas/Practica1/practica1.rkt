#lang plai

(print-only-errors true)

;1 Funcion pow 
(define (pow x y)
 (cond
   [(zero? y) 1]
   [else (* x (pow x (- y 1)))]))

(test (pow 2 10) 1024)
(test (pow 10 10) 10000000000)
(test (pow 5 0) 1)
(test (pow 233 1) 233)
(test (pow (pow 2 2) 16) 4294967296)

;2 Funcion average

(define (average lst)
  (cond
    [(empty? lst) 0]
    [else (/ (sumalist lst) (tamanio lst))]))

;Funcion sumalist que suma todos los elementos de una lista, auxiliar para average
(define (sumalist lst)
  (cond
    [(empty? lst) 0]
    [else (+ (car lst) (sumalist (cdr lst)))]))
;Funcion para sacar el numero de elementos de una lista, auxiliar para average
(define (tamanio lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (tamanio (cdr lst)))]))


(test (average '()) 0)
(test (average '(1 2 3 4 5 6 7 8 9 10)) 5.5)
(test (average '(3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)) 3)
(test (average '(123 456 789 101112)) 25620)
(test (average '(654321 9 70 600 5000 40000 300000)) 142857.142857143)


;3 Funcion primes

(define (primes n)
  (primeAux n '()))

(define (primeAux n ls)
  (cond
      [(> 2 n) ls]
      [(esPrimo? n)(primeAux (- n 1) (cons n ls))]
      [else (primeAux (- n 1) ls)]))

(define (esPrimo? n)
  (cond
      [(< n 2) #f]
      [(esPrimoAux n (- n 1)) #t]
      [else #f]))

(define (esPrimoAux n m)
  (cond
    [(= m 1) #t]
    [(= (modulo n m) 0) #f]
    [else (esPrimoAux n (- m 1))]
   ))
(test (primes 100) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
(test (primes 5) '(2 3 5))
(test (primes 0) '() )
(test (primes 7) '(2 3 5 7) )
(test (primes 12) '(2 3 5 7 11) )
;4 Funcion zip
(define (zip lst1 lst2)
  (cond
    [(empty? lst1) '()]
    [(empty? lst2) '()]
    [else (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))]))

(test (zip '(1 2) '(3 4)) '((1 3)(2 4)))
(test (zip '(6 5 4 2 2 3 4) '()) '())
(test (zip '() '(324 234 234 235 555 2)) '())
(test (zip '(1 2 3 4 5 6) '(1 2 4 45 3)) '((1 1) (2 2) (3 4) (4 45) (5 3)))
(test (zip '(9 8 7 6 5 4 3 2 1) '(1 2 3 4 5 6 7 8 9)) '((9 1) (8 2) (7 3) (6 4) (5 5) (4 6) (3 7) (2 8) (1 9)))

;5 Funcion reduce

(define (reduce foo ls)
  (cond
    [(empty? ls) ls]
    [else (reduceAux foo (cdr ls) (car ls))]))

(define (reduceAux foo ls acc)
  (cond
    [(empty? ls) acc]
    [else (reduceAux foo (cdr ls) (foo (car ls) acc))]))

;(test (reduce pow '(2 2 2 2 2 2 2))) trono

;6 Funcion mconcat

(define (mconcat lst1 lst2)
  (cond
    [(empty? lst2) lst1]
    [(empty? lst1) lst2]
    [else (mconcatAux (reversa lst1 '()) lst2)]))

(define (mconcatAux lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else (mconcatAux (cdr lst1) (cons (car lst1) lst2))]))

(define (reversa lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else(reversa (cdr lst1)(cons (car lst1) lst2))]))

(test (mconcat '(1 2) '(3 4)) '(1 2 3 4))
(test (mconcat '(1 2 3 4 5 6) '()) '(1 2 3 4 5 6))
(test (mconcat '() '(3 4 d s w)) '(3 4 d s w))
(test (mconcat '() '()) '())
(test (mconcat '(a b c d e) '(1 2 3 4 5)) '(a b c d e 1 2 3 4 5))

;7 Funcion mmap

(define (mmap foo ls)
  (cond
    [(empty? ls) '()]
    [else (cons(foo (car ls)) (mmap foo (cdr ls)))]))

;8 Funcion mfilter
(define mfilter
 (lambda (pred lst)
  (cond 
     [(null? lst) '()]
     [(pred (car lst)) (cons (car lst) (mfilter pred (cdr lst)))]
     [else (mfilter pred (cdr lst))])))

(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))
(test (mfilter (lambda (l) (equal? l 'a)) '(1 a 3 4 5 b c)) '(a))
(test (mfilter (lambda (l) (<= l 10)) '(10 2 30 4 50 6 11)) '(10 2 4 6))
(test (mfilter (lambda (l) (equal? l 'perro)) '(1 ds perro 4 erro cosa perro)) '(perro perro))
(test (mfilter (lambda (l) (list? l)) '(1 '() 3 '(4 5) 6 7)) '('() '(4 5)))


;9 Funcion any?

(define (any? foo ls)
  (cond
   [(empty? ls) #f ]
   [(foo (car ls)) #t]
   [else (any? foo (cdr ls))]))

;10 Funcion every?
(define (every? foo lst)
 (cond
   [(empty? lst) #t]
   [(foo (car lst)) (every? foo (cdr lst)) ]
   [else #f]))

(test (every? symbol? '(a b c d e 1)) #f)
(test (every? symbol? '(a b c d e f)) #t)
(test (every? number? '(1 2 3 4 5 empty)) #f)
(test (every? symbol? '()) #t)
(test (every? number? '(23 3545 .5 3/5)) #t)

;11 Funcion mpowerset
;El algoritmo funcionara quitando elementos
;de las listas del tamaño inmediato mas grande
;de manera que se generan muchas listas repetidas
;al eliminar las repetidas desde las que tengan pocos
;elementos hasta las que tengan mas, quedaran en orden.
;y nos quedara el conjunto potencia.

(define (mpowerset ls)
  (cleanSet (reversa ( powersetAux (list ls) (list ls) ) '())))


(define (cleanSet ls)
  (cond
    [(empty? ls) '()]
    [(inList? (car ls) (cdr ls)) (cleanSet (cdr ls))]
    [else (cons (car ls) (cleanSet (cdr ls)))]))

(define (inList? a ls)
  (cond
    [(empty? ls) #f]
    [(mequalList? a (car ls)) #t]
    [else (inList? a (cdr ls))]))


(define (powersetAux ls accResult)
  (cond
    [(empty? ls) accResult]
    [else (powersetAux (mconcat (cdr ls) (individualList (car ls) '())) (mconcat accResult (individualList (car ls) '())))]))


(define (mequalList? ls1 ls2)
  (cond
   [(and (empty? ls1) (empty? ls2)) #t]
   [(or (empty? ls1) (empty? ls2)) #f]
   [(and (symbol? (car ls1)) (and (symbol? (car ls2)) (symbol=? (car ls1) (car ls2)))) (mequalList? (cdr ls1)(cdr ls2))]
   [(and (number? (car ls1)) (and (number? (car ls2)) (= (car ls1)(car ls2)))) (mequalList? (cdr ls1)(cdr ls2))]
   [else #f]
  ))

(define (individualList ls extraAcc)
  (cond
   [(empty? ls) '()]
   [else (mconcat (list (mconcat extraAcc (cdr ls)))  (individualList (cdr ls) (mconcat extraAcc (list(car ls)))))]
   ))

(test (mpowerset '()) '(()))
(test (mpowerset '((1 a 3) ())) '(() ((1 a 3)) (()) ((1 a 3) ())))
(test (mpowerset '(1 b 2)) '(() (1) (b) (2) (1 b) (1 2) (b 2) (1 b 2)))
(test (mpowerset '(a b c d)) '(() (a) (b) (c) (d) (a b) (a c) (a d) (b c) (b d) (c d) (a b c) (a b d) (a c d) (b c d) (a b c d)))
(test (mpowerset '(1 2 3 4 5)) '(() (1) (2) (3) (4) (5) (1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)
                                    (1 2 3) (1 2 4) (1 2 5) (1 3 4) (1 3 5) (1 4 5) (2 3 4) (2 3 5) (2 4 5) (3 4 5)
                                    (1 2 3 4) (1 2 3 5) (1 2 4 5) (1 3 4 5) (2 3 4 5) (1 2 3 4 5)))