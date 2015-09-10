#lang plai

(print-only-errors true)

;Seccion 1

;1 MArray
(define-type Array
  [MArray (n any/c) (lst list?)])

;(test (MArray 5 '(1 2 3 4 5)) (MArray 5 '(1 2 3 4 5)) )

;2 MList
(define-type MList
  [MEmpty]
  [MCons (n any/c) (l MList?)])

(test (MEmpty) (MEmpty))
(test (MCons 1(MCons 2 (MCons 3 (MEmpty)))) (MCons 1(MCons 2 (MCons 3 (MEmpty)))) )


;3 NTree



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

(define (setvalueA arr pos val)
  (type-case Array arr
    [MArray (n lst) (cond
                   [(empty? lst) lst]
                   [(= 0 pos) (sust lst val)]
                   [(> pos 0) (cons (car lst) (setvalueA (MArray (- n 1) (cdr lst)) (- pos 1) val))]
                   [else "Error index"])]))
    
(define (sust lst val)
  (cons val (cdr lst)))
    
   ; [arr (0 arraux) (sust (car arraaux) val)]
   ; [arr (n arraux) (setvalueA arraux (- pos 1))]))   

#|
(test (setvalueA (MArray 1 '(10)) 0 1) (MArray 1 '(1)))
(test (setvalueA (MArray 6 '(1 2 3 4 5 6)) 4 200) (MArray 6 '(1 2 3 4 200 6)))
(test (setvalueA (MArray 5 '(z x y w v))  3 'd) (MArray 5 '(z x y d v)))
(test (setvalueA (MArray 4 '(1 b 3 d))  1 1) (MArray 4 '(1 1 3 d)))
;(test (setvalueA (MArray 4 '(1 2 3 4)) 4 1) (. . setvalueA: Out of bounds)) Verificar sintaxis
|#

;7 MArray2MList
(define (MArray2MList ma)
  (cond
    [(empty? (MGetElements ma)) (MEmpty)]
    [else (MCons (car (MGetElements ma)) (MArray2MList (MArray (MGetSize ma) (cdr (MGetElements ma)))))]))


(define (MGetElements ma)
  (type-case Array ma
    [MArray (n lst) lst]
    ))

(define (MGetSize ma)
  (type-case Array ma
    [MArray (n lst) n]
    ))


(test (MArray2MList (MArray 1 '(1))) (MCons 1 (MEmpty)))
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 100 '(a b c))) (MCons 'a (MCons 'b (MCons 'c (MEmpty)))))
(test (MArray2MList (MArray 2 '(2 2))) (MCons 2 (MCons 2 (MEmpty))))
(test (MArray2MList (MArray 6 '(s f gh 3))) (MCons 's (MCons 'f (MCons 'gh (MCons 3 (MEmpty))))))

;8 printML

(define (printML ml)
  (type-case MList ml
    [MEmpty () "[]"]
    [else (sacaElem ml)]))

(define (sacaElem mls)
  (type-case MList mls
    [MEmpty () empty]
    [MCons (n ls) (cons n (sacaElem ls))]))
#|Le falta el formato pero si sirve!!!!!!
(test (printML (MEmpty)) "[]")
(test (printML (MCons 1 (MEmpty))) "[7]")
(test (printML (MCons  100 (MCons  1000 (MEmpty)))) "[100, 1000]")
(test (printML (MCons  'a (MCons  'z (MEmpty)))) "[a, z]");Revisar sintaxis
(test (printML (MCons (MCons  1 (MCons  2 (MEmpty))) (MCons (MCons  3 (MCons  4 (MEmpty))) (MEmpty)))) "[[1, 2],[3, 4]]")
|#

;9 concatML
(define (concatML ml1 ml2)
  (type-case MList ml1
    [MEmpty () ml2]
    [MCons (e ls) (MCons e (concatML ls ml2))]
    ))

(test (concatML (MEmpty) (MEmpty)) (MEmpty))
(test (concatML (MCons 1 (MEmpty)) (MEmpty)) (MCons 1 (MEmpty)))
(test (concatML (MEmpty) (MCons 1 (MEmpty))) (MCons 1 (MEmpty)))
(test (concatML (MCons 'a (MCons 'b (MEmpty))) (MCons 'c (MCons 'd (MEmpty)))) (MCons 'a (MCons 'b (MCons 'c (MCons 'd (MEmpty))))))
(test (concatML (MCons 1 (MCons 'b (MEmpty))) (MCons 2 (MCons 'c (MCons 3 (MCons 'f (MCons 4 (MCons 'h (MEmpty)))))))) 
                (MCons 1 (MCons 'b (MCons 2 (MCons 'c (MCons 3 (MCons 'f (MCons 4 (MCons 'h (MEmpty))))))))))

;10 lengthML

(define (lengthML  ml)
  (type-case MList ml
      [MEmpty () 0]
      [MCons (x xs) (+ 1 (lengthML xs))]))
#|
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 100 (MEmpty))) 1)
(test (lengthML (MCons 'a (MCons 'b (MCons 'c (MEmpty))))) 3)
(test (lengthML (MCons (MCons 'a (MCons 'b (MCons 'c (MEmpty)))) (MCons 'b (MCons 'c (MEmpty)))) ) 3)
(test (lengthML (MCons 1 (MCons 2 (MCons 3 (MCons 4 (MCons 5 (MCons 6 (MCons 7 (MCons 8 (MCons 9 (MCons 10 (MCons 11 (MCons 12 (MEmpty)))))))))))))) 12)
|#

;11 mapML

(define (mapML foo ml)
  (type-case MList ml
    [MEmpty () ml]
    [MCons (e ls) (MCons (foo e) (mapML foo ls))]))

(test (mapML (lambda (x) (- x x)) (MEmpty)) (MEmpty))
(test (mapML number? (MCons 1 (MCons 'a (MCons 2 (MEmpty))))) (MCons #t (MCons #f (MCons #t (MEmpty)))))
(test (mapML (lambda (x) (- x 10)) (MCons 1 (MCons 2 (MCons 3 (MEmpty))))) (MCons -9 (MCons -8 (MCons -7 (MEmpty)))))
(test (mapML add1 (MCons 1 (MCons 2 (MCons 3 (MEmpty))))) (MCons 2 (MCons 3 (MCons 4 (MEmpty)))))
(test (mapML not (MCons #t (MCons #f (MCons '#t (MCons #f (MCons #t (MEmpty))))))) (MCons #f (MCons #t (MCons #f (MCons #t (MCons #f (MEmpty)))))))

;12 filterML
(define (filterML foo lst)
  (type-case MList lst
    [MEmpty () lst]
    [MCons (e ls) (cond
                    [(foo e) (MCons e (filterML foo ls))]
                    [else (filterML foo ls)])]))
#|
(test (filterML zero? (MCons 2 (MCons 1 (MEmpty)))) (MEmpty))
(test (filterML list? (MEmpty)) (MEmpty))
(test (filterML (lambda (x) (not (zero? x))) (MCons 0 (MCons 2 (MCons 0 (MCons 1 (MEmpty))))) (MCons 2 (MCons 1 (MEmpty))))
(test (filterML (lambda (x) (not (number? x))) (MCons 2 (MCons 6456 (MCons -10 (MCons 'z (MCons 30 (MEmpty))))))) (MCons 'z (MEmpty)))
(test (filterML (lambda (x) (< x 0)) (MCons -2 (MCons .000001 (MCons -1 (MEmpty))))) (MCons -2 (MCons -1 (MEmpty))))
|#

;Definitions for problems
(define-type Coordinates
  [GPS (lat number?)
       (long number?)])

(define-type Location
  [building (name string?)
            (loc GPS?)])

;; Coordenadas GPS
(define gps-satelite (GPS 19.510482 -99.23411900000002))
(define gps-ciencias (GPS 19.3239411016 -99.179806709))
(define gps-zocalo (GPS 19.432721893261117 -99.13332939147949))
(define gps-perisur (GPS 19.304135 -99.19001000000003))

(define plaza-satelite (building "Plaza Satelite" gps-satelite))
(define ciencias (building "Facultad de Ciencias" gps-ciencias))
(define zocalo (building "Zocalo" gps-zocalo))
(define plaza-perisur (building "Plaza Perisur" gps-perisur))

(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))

;13 haversine

(define (haversine coor1 coor2)
  (let ([pi180 (/  pi 180)])
  (let ([delta-lat (- (* pi180 (get-Lat coor2)) (* pi180 (get-Lat coor1)))])
  (let ([delta-long (- (* pi180 (get-Long coor2)) (* pi180 (get-Long coor1)))])
  (let ([a (+ (* (sin (/ delta-lat 2)) (sin (/ delta-lat 2))) (* (cos (* pi180 (get-Lat coor2))) (cos (* pi180 (get-Lat coor1))) (sin (/ delta-long 2)) (sin (/ delta-long 2)))) ])
  (let ([c (* 2 (atan (sqrt a) (sqrt (- 1 a))) (atan (sqrt a) (sqrt (- 1 a))))])
  (* 6373 c)
  ))))))


(define (get-Lat coor)
  (type-case Coordinates coor
    [GPS (la lo) la]
    ))

(define (get-Long coor)
  (type-case Coordinates coor
    [GPS (la lo) lo]
    ))

;14 gps-coordinates


;15 closest-building


;16 building-at-distance


;17 area
(define (area fig)
  (type-case Figure fig
    [Circle (a n) (* pi (* n n))]
    [Square (a n)  (* n n)]
    [Rectangle (a n m) (* n m)]))
#|


|#     


;18 in-figure?
#|
(define (in-figure? fig p)
  (type-case Figure fig
      [Circle (a n) (< n (sqrt (+ (expt (- (saca-x p) (saca-x a)) 2) ((expt (- (saca-y p) (saca-y a)) 2)))))]
      [Square (a n)  (and (and (<= (saca-x a) (saca-x p)) (>= (+ (saca-x a) n) (saca-x p))) (and (<= (- (saca-y a) n) (saca-y p)) (>= (saca-y a) (saca-y p))))]
      [Rectangle (a n m) (* n m)])) 
|#
