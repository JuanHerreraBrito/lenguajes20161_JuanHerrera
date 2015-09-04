#lang plai

(define-type Array
  [MArray (n number?) (lst list?)])

;(test (MArray 5 '(1 2 3 4 5)) (MArray 5 '(1 2 3 4 5)) )

(define-type MList
  [MEmpty]
  [MCons (n number?) (l MList?)])

;(test (MEmpty) (MEmpty))
;(test (MCons 1(MCons 2 (MCons 3 (MEmpty)))) (MCons 1(MCons 2 (MCons 3 (MEmpty)))) )

(define-type NTree
  [TLEmpty]
  ;[NodeN lista de tlemptys que se llama nnodo o una lista e cosas que se llama nnoden
  )