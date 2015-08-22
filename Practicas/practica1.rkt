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

;8 Funcion mfilter

;9 Funcion any?

;10 Funcion every?

;11 Funcion mpowerset
