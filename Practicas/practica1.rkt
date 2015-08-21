#lang plai

(print-only-errors true)

;Funcion pow 
(define (pow x y)
 (cond
   [(zero? y) 1]
   [else (* x (pow x (- y 1)))]))

;Funcion average
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

;Funcion primes

;Funcion zip

;Funcion reduce

;Funcion mconcat
(define (mconcat lst1 lst2)
  (cond
    [(empty? lst2) lst1]
    [else (mconcat (pega-fin lst1 (car lst2)) (cdr lst2))]))

(define (pega-fin lst1 lst2)
  (cond
    [(empty? lst2) lst1]
    [else (append lst1 (car lst2))]))
;Error en la funcion pega-fin

;Funcion mmap

;Funcion mfilter

;Funcion any?

;Funcion every?

;Funcion mpowerset
