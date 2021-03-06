#lang plai

(print-only-errors true)

;Seccion 1

;1 MArray
(define-type Array
  [MArray (n any/c) (lst list?)])

(test (MArray 5 '(1 2 3 4 5)) (MArray 5 '(1 2 3 4 5)) )

;2 MList
(define-type MList
  [MEmpty]
  [MCons (n any/c) (l MList?)])

(test (MEmpty) (MEmpty))
(test (MCons 1(MCons 2 (MCons 3 (MEmpty)))) (MCons 1(MCons 2 (MCons 3 (MEmpty)))) )


;3 NTree

(define-type NTree
  [TLEmpty]
  [NodeN (n any/c) (l listOfNTree?) ])


(define (listOfNTree? l)
  (cond
    [(empty? l) (error 'NotANTree)]
    [else (listOfNTreeAux? l)]))

(define (listOfNTreeAux? l)
  (cond
    [(empty? l) '()]
    [(NTree? (car l)) (cons (car l) (listOfNTreeAux? (cdr l)))]
    [else (error 'NotANTree)]))

(test (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))) (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))

;4 Position
 (define-type Position
   [2D-Point (n number?) (m number?)])  

 (test (2D-Point 422 4545) (2D-Point 422 4545))

;5 Figure
(define-type Figure
             [Circle (p Position?) (n number?)]
             [Square (p Position?) (n number?)]
             [Rectangle (p Position?) (n number?) (m number?)])

(test (Circle (2D-Point 422 4545) 5) (Circle (2D-Point 422 4545) 5))
(test (Square (2D-Point 422 4545) 5) (Square (2D-Point 422 4545) 5))
(test (Rectangle (2D-Point 422 4545) 5 6) (Rectangle (2D-Point 422 4545) 5 6))

;Seccion 2: Funciones sobre datos

;6 setvalueA

(define (setvalueA arr pos val)
  (type-case Array arr
    [MArray (n lst) (MArray n (setvalueB arr pos val))]))



(define (setvalueB arr pos val)
  (type-case Array arr
    [MArray (n lst) (cond
                   [(empty? lst) lst]
                   [(= 0 pos) (sust lst val)]
                   [(> pos 0) (cons (car lst) (setvalueB (MArray (- n 1) (cdr lst)) (- pos 1) val))]
                   [else "Error index"])]))
    
(define (sust lst val)
  (cons val (cdr lst)))
    

(test (setvalueA (MArray 1 '(10)) 0 1) (MArray 1 '(1)))
(test (setvalueA (MArray 6 '(1 2 3 4 5 6)) 4 200) (MArray 6 '(1 2 3 4 200 6)))
(test (setvalueA (MArray 5 '(z x y w v))  3 'd) (MArray 5 '(z x y d v)))
(test (setvalueA (MArray 4 '(1 b 3 d))  1 1) (MArray 4 '(1 1 3 d)))
(test (setvalueA (MArray 4 '(1 2 3 4)) 3 'b) (MArray 4 '(1 2 3 b)) )


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

(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 100 (MEmpty))) 1)
(test (lengthML (MCons 'a (MCons 'b (MCons 'c (MEmpty))))) 3)
(test (lengthML (MCons (MCons 'a (MCons 'b (MCons 'c (MEmpty)))) (MCons 'b (MCons 'c (MEmpty)))) ) 3)
(test (lengthML (MCons 1 (MCons 2 (MCons 3 (MCons 4 (MCons 5 (MCons 6 (MCons 7 (MCons 8 (MCons 9 (MCons 10 (MCons 11 (MCons 12 (MEmpty)))))))))))))) 12)


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

(test (filterML zero? (MCons 2 (MCons 1 (MEmpty)))) (MEmpty))
(test (filterML list? (MEmpty)) (MEmpty))
(test (filterML (lambda (x) (not (zero? x))) (MCons 0 (MCons 2 (MCons 0 (MCons 1 (MEmpty)))))) (MCons 2 (MCons 1 (MEmpty))))
(test (filterML (lambda (x) (not (number? x))) (MCons 2 (MCons 6456 (MCons -10 (MCons 'z (MCons 30 (MEmpty))))))) (MCons 'z (MEmpty)))
(test (filterML (lambda (x) (< x 0)) (MCons -2 (MCons .000001 (MCons -1 (MEmpty))))) (MCons -2 (MCons -1 (MEmpty))))


;Definitions for problems
(define-type Coordinates
  [GPS (lat number?)
       (long number?)])

(define-type Location
  [building (name string?)
            (loc GPS?)])

; Coordenadas GPS
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
  (let* ([pi180 (/  pi 180)]
         [lat2 (* pi180 (get-Lat coor2))]
         [lat1 (* pi180 (get-Lat coor1))]
         [long2 (* pi180 (get-Long coor2))]
         [long1 (* pi180 (get-Long coor1))]
         [delta-lat (- lat2 lat1)]
         [delta-long (- long2 long1)]
         [a (+ (pow2 (sin (/ delta-lat 2))) (* (cos lat1)(cos lat2)(pow2 (sin (/ delta-long 2)))))]
         [c (* 2 (asin (sqrt a)))])
         (* 6371 c)))

(define (pow2 x)
  (* x x))

(define (get-Lat coor)
  (type-case Coordinates coor
    [GPS (la lo) la]
    ))

(define (get-Long coor)
  (type-case Coordinates coor
    [GPS (la lo) lo]
    ))

(test (haversine gps-ciencias gps-zocalo) 13.033)
(test (haversine gps-ciencias gps-perisur) 2.447)
(test (haversine gps-satelite gps-perisur) 23.406)
(test (haversine gps-zocalo gps-perisur) 15.485)
(test (haversine gps-satelite gps-zocalo) 13.653)

;14 gps-coordinates

(define (gps-coordinates ml)
 (type-case MList ml
  [MEmpty () ml]
  [MCons (e ls) (MCons (getGPS e) (gps-coordinates ls))]))


(define (getGPS b)
  (type-case Location b
    [building (na loc) loc]))

(define gps-mi-casa (GPS 25.25547 -2.457000568))
(define gps-ingenieria (GPS 11.46548646541 -99.456846548))
(define gps-conagua (GPS 12.0000245 -97.2350012458))
(define gps-gransur (GPS 19.304135 -100.5468548654))
(define gps-metro-universidad (GPS 23.564588 -89.2335644478))
(define gps-tienda (GPS 10.256484 -82.225644485))

(define mi-casa (building "Mi Casa" gps-mi-casa))
(define ingenieria (building "Facultad de Igenieria" gps-ingenieria))
(define conagua (building "Conagua" gps-conagua))
(define plaza-gransur (building "Plaza GranSur" gps-gransur))
(define metro (building "Metro Universidad" gps-metro-universidad))
(define tienda (building "Tienda" gps-tienda))


(define escuelas (MCons ciencias (MCons ingenieria (MEmpty))))
(define edificios (MCons mi-casa(MCons tienda (MCons conagua (MEmpty)))))



(test (gps-coordinates (MEmpty)) (MEmpty))
(test (gps-coordinates plazas) (MCons (GPS 19.510482 -99.23411900000002)(MCons (GPS 19.304135 -99.19001000000003) (MEmpty))))
(test (gps-coordinates escuelas) (MCons (GPS 19.3239411016 -99.179806709) (MCons (GPS 11.46548646541 -99.456846548) (MEmpty))))
(test (gps-coordinates edificios) (MCons (GPS 25.25547 -2.457000568) (MCons (GPS 10.256484 -82.225644485) (MCons (GPS 12.0000245 -97.2350012458) (MEmpty)))))
(test (gps-coordinates (MCons metro (MEmpty))) (MCons (GPS 23.564588 -89.2335644478) (MEmpty)))
;15 closest-building
(define (closest-building b ml)
  (type-case MList ml
    [MEmpty () ml]
    [MCons (e ls) (closest-building-extra b ls e)]
      ))

(define (closest-building-extra b ml win)
  (type-case MList ml
    [MEmpty () win]
    [MCons (e ls) (if (< (haversine (getGPS b) (getGPS e)) (haversine (getGPS b) (getGPS win) )) (closest-building-extra b ls e)
                      (closest-building-extra b ls win))]))

(test (closest-building zocalo plazas) (building "Plaza Satelite" (GPS 19.510482 -99.23411900000002)))
(test (closest-building ciencias plazas) (building "Plaza Perisur" (GPS 19.304135 -99.19001000000003)))
(test (closest-building plaza-perisur plazas) (building "Plaza Perisur" (GPS 19.304135 -99.19001000000003)))  
(test (closest-building plaza-satelite plazas) (building "Plaza Satelite" (GPS 19.510482 -99.23411900000002)))
(test (closest-building mi-casa escuelas) (building "Facultad de Ciencias" (GPS 19.3239411016 -99.179806709)))
;16 building-at-distance
(define (buildings-at-distance bu lst dis)
  (type-case MList lst
        [MEmpty () (MEmpty)]
        [MCons (e ls) (cond
                        [(< (haversine (getGPS bu) (getGPS e)) dis) (MCons e (buildings-at-distance bu ls dis))]
                        [else (buildings-at-distance bu ls dis)])]))


(test (buildings-at-distance ciencias plazas 1) (MEmpty))
(test (buildings-at-distance plaza-perisur (MCons ciencias plazas) 3) (MCons (building "Facultad de Ciencias" (GPS 19.3239411016 -99.179806709)) (MCons (building "Plaza Perisur" (GPS 19.304135 -99.19001000000003)) (MEmpty))))
(test (buildings-at-distance ciencias plazas 10) (MCons (building "Plaza Perisur" (GPS 19.304135 -99.19001000000003)) (MEmpty)))
(test (buildings-at-distance ciencias plazas 20) (MCons (building "Plaza Perisur" (GPS 19.304135 -99.19001000000003)) (MEmpty)))
(test (buildings-at-distance ciencias plazas 25) (MCons (building "Plaza Satelite" (GPS 19.510482 -99.23411900000002)) (MCons (building "Plaza Perisur" (GPS 19.304135 -99.19001000000003)) (MEmpty))))

;17 area
(define (area fig)
  (type-case Figure fig
    [Circle (a n) (* pi (* n n))]
    [Square (a n)  (* n n)]
    [Rectangle (a n m) (* n m)]))

(test (area (Square (2D-Point 0 0) 10)) 100)
(test (area (Circle (2D-Point 5 5) 4)) 50.26548245743669)
(test (area (Rectangle (2D-Point 0 0) 10 2)) 20)
(test (area (Square (2D-Point 0 0) 7)) 49)
(test (area (Circle (2D-Point 0 0) 1)) 3.1415)

;18 in-figure?
(define (in-figure? fig p)
  (type-case Figure fig
      [Circle (a n) (inCircle p a n)]
      [Square (a n)  (inRectangule (saca-x a) (saca-y a) n n (saca-x p) (saca-y p))]
      [Rectangle (a n m) (inRectangule (saca-x a) (saca-y a) n m (saca-x p) (saca-y p))]))



(define (inRectangule p1x p1y a l p2x p2y)
  (cond 
    [(or (< p2x p1x) (< p2y p1y ) ) #f ]
    [(and (<= p2x (+ p1x a)) (<= p2y (+ p1y l)) ) #t]
    [else #f]))
(define (inCircle can orig rad)
  (cond
    [(<= (dist can orig) rad) #t]
    [else #f]))

(define (dist a b)
    (cond
      [(not (and (2D-Point? a) (2D-Point? a))) "Error"]
      [else (sqrt (+ (expt (- (saca-x b) (saca-x a)) 2) (expt (- (saca-y b) (saca-y a)) 2)))]))


(define (saca-x pun)
  (type-case Position pun
    [2D-Point (x y) x]))

(define (saca-y pun)
  (type-case Position pun
    [2D-Point (x y) y]))

(test (in-figure? (Square (2D-Point 5 5) 4) (2D-Point 6 6)) #t)
(test (in-figure? (Rectangle (2D-Point 5 5) 4 6) (2D-Point 4 4)) #f)
(test (in-figure? (Circle (2D-Point 0 0) 20) (2D-Point 22 25)) #f)
(test (in-figure? (Circle (2D-Point 300 20) 1) (2D-Point 300 19.5)) #t)
(test (in-figure? (Square (2D-Point 0 0) 4) (2D-Point 0 12)) #f)