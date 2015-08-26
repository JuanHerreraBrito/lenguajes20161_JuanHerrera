#lang plai

;QuickSort
(define (quick-sort lst)
  (cond
    [(empty? lst) lst]
    [(empty? (cdr lst)) lst]
    [else (append(quick-sort(smaller (car lst) (cdr lst))) (cons(car lst) (quick-sort(bigger (car lst) (cdr lst))))) ]))
  
  
(define (smaller n lst)
  (cond
    [(empty? lst) lst]
    [(<= (car lst) n) (cons (car lst) (smaller n (cdr lst)))]
    [else (smaller n (cdr lst))]))
  
  (define (bigger n lst)
  (cond
    [(empty? lst) lst]
    [(> (car lst) n) (cons (car lst) (bigger n (cdr lst)))]
    [else (bigger n (cdr lst))]))
  
 (test (quick-sort '(2 5 7 1 10 3)) '(1 2 3 5 7 10))
 (test (quick-sort '(2 5 7 1 10 3 23 56 1 2 52 9 1000)) '(1 1 2 2 3 5 7 9 10 23 52 56 1000))
 