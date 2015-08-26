#lang plai

(print-only-errors true)

;1 Funcion pow 
(define (pow x y)
 (cond
   [(zero? y) 1]
   [else (* x (pow x (- y 1)))]))

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
  
;4 Funcion zip
(define (zip lst1 lst2)
  (cond
    [(empty? lst1) (list)]
    [(empty? lst2) (list)]
    [else '((car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2))]))


;5 Funcion reduce

(define (reduce foo ls)
  (cond
    [(empty? ls) ls]
    [else (reduceAux foo (cdr ls) (car ls))]))

(define (reduceAux foo ls acc)
  (cond
    [(empty? ls) acc]
    [else (reduceAux foo (cdr ls) (foo (car ls) acc))]))

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

;7 Funcion mmap

(define (mmap foo ls)
  (cond
    [(empty? ls) '()]
    [else (cons(foo (car ls)) (mmap foo (cdr ls)))]))

;8 Funcion mfilter

;9 Funcion any?

(define (any? foo ls)
  (cond
   [(empty? ls) #f ]
   [(foo (car ls)) #t]
   [else (any? foo (cdr ls))]))

;10 Funcion every?

;11 Funcion mpowerset
;El algoritmo funcionara quitando elementos
;de las listas del tamaño inmediato mas grande
;de manera que se generan muchas listas repetidas
;al eliminar las repetidas desde las que tengan pocos
;elementos hasta las que tengan mas, quedaran en orden.
;y nos quedara el conjunto potencia.
(define (mpowerset ls)
  (cleanSet ( powersetAux (list ls) (list ls) (list ls))))


(define (flipList ls)
  #|(quitaRepetidos ())|#5)

(define (cleanSet ls)
  #|(quitaRepetidos ())|#5)

(define (powersetAux ls accOperative accResult)
  (cond
    [(empty? ls) accResult]
    [else (powersetAux (mconcat (cdr ls) (individualList (car ls) '())) accOperative (mconcat accResult (individualList (car ls) '())))]))


(define (mequal? ls1 ls2)
  (cond
   [(and (empty? ls1) (empty? ls2)) #t]
   [(or (empty? ls1) (empty? ls2)) #f]
   [(= (car ls1)(car ls2)) (mequal? (cdr ls1)(cdr ls2))]
   [else #f]
  ))

(define (individualList ls extraAcc)
  (cond
   [(empty? ls) '()]
   [else (mconcat (list (mconcat extraAcc (cdr ls)))  (individualList (cdr ls) (mconcat extraAcc (list(car ls)))) )]
   ))