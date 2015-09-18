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

;(define (get-zone t z)
;  (cond
;    [(equal? t 'resting) (car z)]
;    [(equal? t 'warm-up) (cadr z)]
;    [(equal? t 'fat-burning) (caddr z)]
;    [(equal? t 'aerobic) (cadddr z)]
;    [(equal? t 'anaerobic) (car(cddddr z))]
;    [(equal? t 'maximum) (cdr(cddddr z))]))
    


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
(define (create-trackpoint ls z) #t)

;5 total-distance
(define (total-distance tl) #t)

;6 average-hr
(define (average-hr tl) #t)

;7 max-hr
(define (max-hr tl) #t)

; 8 collapse-trackpoints
(define (collapse-trackpoints tl e) #t)

;BTree
; 1 ninBT
(define (ninBT bt) #t)

; 2 nlBT
(define (nlBT bt) #t)

; 3 nnBT
(define (nnBT bt) #t)

; 4 mapBT
(define (mapBT foo bt) #t)


; arbol base
; 1 preoderBT
(define (preoderBT ab) #t)

; 2 inorderBT
(define (inorderBT ab) #t)

; 3 posorderBT
(define (posorderBT ab) #t)