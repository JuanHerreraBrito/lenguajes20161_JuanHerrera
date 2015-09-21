#lang plai

(require "practica3-base.rkt")

;1 zones
(define (zones n m)
  (cons (resting n (+ n (* (- m n) 0.5) -1)) (zonesAux n m (- m n) 0)))

(define (zonesAux n m r i)
  (cond
    [(<= i 0) (cons (warm-up (get-Min n m r i) (get-Max n m r i)) (zonesAux n m r (+ i 1)))]
    [(<= i 1) (cons (fat-burning (get-Min n m r i) (get-Max n m r i)) (zonesAux n m r (+ i 1)))]
    [(<= i 2) (cons (aerobic (get-Min n m r i) (get-Max n m r i)) (zonesAux n m r (+ i 1))) ]
    [(<= i 3) (cons (anaerobic (get-Min n m r i) (get-Max n m r i)) (zonesAux n m r (+ i 1)))]
    [(<= i 4) (cons (maximum (get-Min n m r i) (+ (get-Max n m r i) 1)) (zonesAux n m r (+ i 1)))]
    [else '()]))


(define (get-Min n m r i)
  (+ n (* r (+ 0.5 (* 0.1 i)))))

(define (get-Max n m r i)
  (+ n (* r (+ 0.5 (* 0.1 (+ i 1)))) -1))

(define my-zones (zones 50 180))
#|
(test (zones 50 180) (list (resting 50 114.0) (warm-up 115.0 127.0) (fat-burning 128.0 140.0) (aerobic 141.0 153.0) (anaerobic 154.0 166.0) (maximum 167.0 180.0)) )
(test (zones 100 180) (list (resting 100 139.0) (warm-up 140.0 147.0) (fat-burning 148.0 155.0) (aerobic 156.0 163.0) (anaerobic 164.0 171.0) (maximum 172.0 180.0)))
(test (zones 100 105) (list (resting 100 101.5) (warm-up 102.5 102.0) (fat-burning 103.0 102.5) (aerobic 103.5 103.0) (anaerobic 104.0 103.5) (maximum 104.5 105.0)))
(test (zones 85 200) (list (resting 85 141.5) (warm-up 142.5 153.0) (fat-burning 154.0 164.5) (aerobic 165.5 176.0) (anaerobic 177.0 187.5) (maximum 188.5 200.0)) )
(test (zones 85 150) (list (resting 85 116.5) (warm-up 117.5 123.0) (fat-burning 124.0 129.5) (aerobic 130.5 136.0) (anaerobic 137.0 142.5) (maximum 143.5 150.0)))
|#
( define zone-1 (zones 50 180))
( define zone-2 (zones 50 200))

;2 get-zone
(define (get-zone t z) 
  (cond
    [(empty? z) error "no se encontraron zonas"]
    [(equal? (HR-type (car z)) t) (car z)]
    [else (get-zone t (cdr z))]))

(define (HR-type exp)
  (type-case HRZ exp
    [resting (l r) 'resting]
    [warm-up (l r) 'warm-up]
    [fat-burning (l r) 'fat-burning]
    [aerobic (l r) 'aerobic]
    [anaerobic (l r) 'anaerobic]
    [maximum (l r) 'maximum]))
#|
(test (get-zone 'anaerobic zone-1) (anaerobic 154.0 166.0) )
(test (get-zone 'maximum zone-1) (maximum 167.0 180.0))
(test (get-zone maximum zone-1) "no se encontraron zonas")
(test (get-zone 'resting zone-2) (resting 50 124.0))
(test (get-zone 'fat-burning zone-2) (fat-burning 140.0 154.0))
|#
;3 bpm->zone
(define (bpm->zone fcl z)
  (cond
    [(empty? fcl) '()]
    [else (cons (zone-in (car fcl) z) (bpm->zone (cdr fcl) z))]))
    
(define (zone-in n z)
  (cond 
    [(empty? z) error "no se encuentra en las zonas"]
    [(in-rate? (car z) n) (car z)]
    [else (zone-in n (cdr z))]))
  
(define (in-rate? exp n)
  (type-case HRZ exp
    [resting (l r) (cond 
                     [(and (<= l n) (>= r n)) #t]
                     [else #f])]
    [warm-up (l r) (if (and (<= l n) (>= r n)) #t #f)]
    [fat-burning (l r) (if (and (<= l n) (>= r n)) #t #f)]
    [aerobic (l r) (if (and (<= l n) (>= r n)) #t #f)]
    [anaerobic (l r) (if (and (<= l n) (>= r n)) #t #f)]
    [maximum (l r) (if (and (<= l n) (>= r n)) #t #f)])) 
#|  
(test (bpm->zone empty zone-1) '())
(test (bpm->zone '(50 100) zone-1) (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(50 180) zone-1) (list (resting 50 114.0) (maximum 167.0 180.0)))
(test (bpm->zone '(150 180) zone-1) (list (aerobic 141.0 153.0) (maximum 167.0 180.0)))
(test (bpm->zone '(181 185) zone-1) '("no se encuentra en las zonas" "no se encuentra en las zonas"))
|#

;4 create-trackpoints
(define (create-trackpoints ls z) 
  (cond 
    [(empty? ls) '()]
    [else (cons (trackpoint (GPS (first (cadr (car ls))) (second (cadr (car ls)))) (third (car ls)) (car (bpm->zone (list (caddr (car ls))) z)) (car (car ls))) (create-trackpoints (cdr ls) z))]))
#|
(test (create-trackpoints (take raw-data 4) zone-2) (list
 (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 124.0) 1425619654)
 (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 124.0) 1425619655)
 (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 124.0) 1425619658)
 (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 124.0) 1425619662)))

(test (create-trackpoints (take raw-data 4) zone-1) (list
 (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
 (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
 (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
 (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
|#
;5 total-distance
(define (total-distance tl) 
  (cond
    [(empty? tl) 0]
    [(empty? (cdr tl)) 0]
    [else (+ (haversine (get-loc-tp (car tl)) (get-loc-tp (cadr tl))) (total-distance (cdr tl)))]))

(define (get-loc-tp exp)
  (type-case Frame exp
    [trackpoint (l h z u) l]))

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
  (type-case Coordinate coor
    [GPS (la lo) la]
    ))

(define (get-Long coor)
  (type-case Coordinate coor
    [GPS (la lo) lo]
    ))
(define trackpoints (create-trackpoints raw-data my-zones))
(define sample (create-trackpoints (take raw-data 100) my-zones))
(define trackpoints-1 (create-trackpoints (take raw-data 50) zone-1))
(define trackpoints-2 (create-trackpoints (take raw-data 500) zone-2))
(define trackpoints-3 (create-trackpoints (take raw-data 250) zone-2))
#|
(test (total-distance sample) 0.9515265354856569)
(test (total-distance trackpoints) 5.055108373447764)
(test (total-distance trackpoints-1) 0.4610032616049099)
(test (total-distance trackpoints-2) 4.842267156628307)
(test (total-distance trackpoints-3) 2.4314134377480365)
|#
;6 average-hr
(define (average-hr tl)
  (truncate (/ (average-hr-aux tl) (length tl))))

(define (get-hr-tp exp)
  (type-case Frame exp
    [trackpoint (l h z u) h]))

(define (average-hr-aux tl)
  (cond
    [(empty? tl) 0]
    [else (+ (get-hr-tp (car tl)) (average-hr-aux (cdr tl)))]))
#|
(test (average-hr sample) 134)
(test (average-hr trackpoints) 150)
(test (average-hr trackpoints-1) 127)
(test (average-hr trackpoints-2) 149)
(test (average-hr trackpoints-3) 142)
|#
;7 max-hr
(define (max-hr tl) 
  (cond 
    [(empty? tl) 0]
    [else (max (get-hr-tp (car tl)) (max-hr (cdr tl)))]))
#|
(test (max-hr sample) 147)
(test (max-hr trackpoints) 165)
(test (max-hr trackpoints-1) 136)
(test (max-hr trackpoints-2) 165)
(test (max-hr trackpoints-3) 165)
|#
; 8 collapse-trackpoints
(define (collapse-trackpoints tl e)
  (cond
    [(empty? tl) '()]
    [else (collapse-trackpoints-aux (car tl) (cdr tl) e (distance-trackpoints tl))]))

(define (collapse-trackpoints-aux elem tl e dl)
  (cond
    [(empty? tl) (list elem)]
    [(and (< (car dl) e) (equal? (get-hr-tp elem) (get-hr-tp (car tl)))) (collapse-trackpoints-aux (car tl) (cdr tl) e (cdr dl))]
    [else (cons elem (collapse-trackpoints-aux (car tl) (cdr tl) e (cdr dl)))]))


(define (distance-trackpoints tl)
  (cond
    [(empty? tl) '()]
    [(empty? (cdr tl)) '()]
    [else (cons (haversine (get-loc-tp (car tl)) (get-loc-tp (cadr tl))) (distance-trackpoints (cdr tl)))]))

(define track-1 (create-trackpoints (take raw-data 3) my-zones))
(define track-2 (create-trackpoints (take raw-data 5) my-zones))
(define track-3 (create-trackpoints (take raw-data 10) zone-1))
(define track-4 (create-trackpoints (take raw-data 20) zone-2))
(define track-5 (create-trackpoints (take raw-data 2) zone-2))

;BTree
; 1 ninBT
(define (ninBT bt) 
  (type-case BTree bt
    [EmptyBT () 0]
    [BNode (f l v r) (cond
                       [(and (empty-sonBT? l) (empty-sonBT? r)) 0]
                       [else (+ 1 (ninBT l) (ninBT r))])]))

(define (empty-sonBT? bt) 
  (type-case BTree bt
    [EmptyBT () #t]
    [BNode (f l v r) #f]))
(define tree-1 (bnn ebt ebt ebt))
(define tree-2 (bnn (bnn ebt 2 ebt) 1 (bnn ebt 3 ebt)))
(define tree-3 (bnn (bnn (bnn ebt 9 ebt) 10 (bnn ebt 15 ebt)) 3 (bnn (bnn ebt 1 ebt) 7 (bnn ebt 12 ebt))))
(define tree-4 (bnn ebt 9 ebt))
(define tree-5 (bnn (bnn (bnn ebt 4 ebt) 5 (bnn ebt 6 (bnn ebt 7 (bnn ebt 8 ebt)))) 1 (bnn (bnn (bnn ebt 9 ebt) 10 (bnn ebt 15 ebt)) 3 (bnn (bnn ebt 1 ebt) 7 (bnn ebt 12 ebt)))))

(test (ninBT tree-1) 0)
(test (ninBT tree-2) 1)
(test (ninBT tree-3) 3)
(test (ninBT tree-4) 0)
(test (ninBT tree-5) 7)

; 2 nlBT
(define (nlBT bt)
    (type-case BTree bt
      [EmptyBT () 0]
      [BNode (f l v r) (cond
                         [(and (empty-sonBT? l) (empty-sonBT? r)) 1]
                         [else (+ 0 (nlBT l) (nlBT r))])]))

; 3 nnBT
(define (nnBT bt) 
  (type-case BTree bt
    [EmptyBT () 0]
    [BNode (f l v r) (+ 1 (nnBT l) (nnBT r))]))
  
; 4 mapBT
(define (mapBT foo bt)
  (type-case BTree bt
    [EmptyBT () (EmptyBT)]
    [BNode (f l v r) (BNode f (mapBT foo l) (foo v) (mapBT foo r))]))

; arbol base
; 1 preoderBT
(define (preorderBT ab)
  (type-case BTree ab
    [EmptyBT () '()]
    [BNode (f l v r) (cons v (append (preorderBT l) (preorderBT r)))]))

; 2 inorderBT
(define (inorderBT ab)
  (type-case BTree ab
    [EmptyBT () '()]
    [BNode (f l v r) (append (inorderBT l) (cons v (inorderBT r)))]))


; 3 posorderBT
(define (posorderBT ab)
  (type-case BTree ab
    [EmptyBT () '()]
    [BNode (f l v r) (append (posorderBT l) (posorderBT r) (list v))]))

;(printBT (bnn (bnn (bnn (bnn ebt 4 ebt) 5 (bnn ebt 6 (bnn ebt 7 (bnn ebt 8 ebt)))) 1 (bnn ebt 3 ebt)))