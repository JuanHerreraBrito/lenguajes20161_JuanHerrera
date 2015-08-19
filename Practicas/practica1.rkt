#lang plai

(print-only-errors true)

;Funcion pow 
(define (pow x y)
 (cond
   [(zero? y) 1]
   [else (* x (pow x (- y 1)))]))

;Funcion average

;Funcion primes

;Funcion zip

;Funcion reduce

;Funcion mconcat

;Funcion mmap

;Funcion mfilter

;Funcion any?

;Funcion every?

;Funcion mpowerset