#lang plai


;Seccion 1

;1 MArray
(define-type Array
  [MArray (n any/c) (lst list?)])

;(test (MArray 5 '(1 2 3 4 5)) (MArray 5 '(1 2 3 4 5)) )

;2 MList
(define-type MList
  [MEmpty]
  [MCons (n any/c) (l MList?)]);;poner any en lugar de number

(test (MEmpty) (MEmpty))
(test (MCons 1(MCons 2 (MCons 3 (MEmpty)))) (MCons 1(MCons 2 (MCons 3 (MEmpty)))) )


;3 NTree
#|(define-type NTree
  [TLEmpty]
  [NodeN (n any/c) ((ListOfNtree? l) List?)])

(define (ListOfNTree? lst)
  (cond
    [(empty?) #t]
    [((car lst) NTree?)]
   ))

|#

(define-type NTree
  [TLEmpty]
  ;[NodeN lista de tlemptys que se llama nnodo o una lista e cosas que se llama nnoden
  )


;4 Position
 (define-type Position
   [2D-Point (n number?) (m number?)])  

 (test (2D-Point 422 4545) (2D-Point 422 4545))

;5 Figure
(define-type Figure
             [Circle (p 2D-Point?) (n number?)]
             [Square (p 2D-Point?) (n number?)]
             [Rectangle (p 2D-Point?) (n number?) (m number?)])

(test (Circle (2D-Point 422 4545) 5) (Circle (2D-Point 422 4545) 5))
(test (Square (2D-Point 422 4545) 5) (Square (2D-Point 422 4545) 5))
(test (Rectangle (2D-Point 422 4545) 5 6) (Rectangle (2D-Point 422 4545) 5 6))

;Seccion 2: Funciones sobre datos

;6 setvalueA



;7 MArray2MList
;(define (MArray2MList ))

#|
(define (MGetElements mlst)
  (cases Array mlst
    [MArray (n lst) lst]
    [else (error MGetElements "Not a MArray")]))
|#
;8 printML


;9 concatML


;10 lengthML


;11 mapML


;12 filterML


;13 haversine


;14 gps-coordinates


;15 closest-building


;16 building-at-distance


;17 area


;18 in-figure?


