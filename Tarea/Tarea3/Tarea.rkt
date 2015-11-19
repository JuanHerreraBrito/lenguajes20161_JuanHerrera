#lang plai
(define (fib n)
  (if (= n 0) 
      1
      (* n (fib (- n 1)))))
