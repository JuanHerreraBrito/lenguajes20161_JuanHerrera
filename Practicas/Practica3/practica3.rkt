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
  

;4 create-trackpoints
(define (create-trackpoints ls z) 
  (cond 
    [(empty? ls) '()]
    [else (cons (trackpoint (GPS (first (cadr (car ls))) (second (cadr (car ls)))) (third (car ls)) (car (bpm->zone (list (caddr (car ls))) z)) (car (car ls))) (create-trackpoints (cdr ls) z))]))

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

;7 max-hr
(define (max-hr tl) 
  (cond 
    [(empty? tl) 0]
    [else (max (get-hr-tp (car tl)) (max-hr (cdr tl)))]))

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