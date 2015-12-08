#lang racket
(define x (cons 3 4))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))

(define (abs x)(cond ((> x 0) x)
                     ((< x 0)(- x))
                     (else 0)
                     ))
(define (avg a b)(/ (+ a b) 2))

(define (square x)(* x x))

; exercise 2.1
(define (make-rat a b)
  (let ((g ((if (< b 0)  - +)(abs (gcd a b)))))(cons (/ a g)(/ b g)))
)
  
(define (numer x)(car x))
(define (denom x)(cdr x))

; exercise 2.2
(define (make-segment start end)(cons start end))
(define (start-segment line)(car line))
(define (end-segment line)(cdr line))
(define (make-point x y)(cons x y))
(define (x-point p)(car p))
(define (y-point p)(cdr p))
(define (midpoint-segment line)
  (let ((st(start-segment line)) (ed (end-segment line)))
     (make-point (avg (x-point st)(x-point ed)) (avg (y-point st)(y-point ed)))
  )
    )
 (define (print-point p)
(newline)
(display "(")
(display (x-point p))
(display ",")
(display (y-point p))
(display ")"))

; exercise 2.3
; up and down would be lines
(define (make-rect up down)(cons up down))
(define (up-bound rect)(car rect))
(define (down-bound rect)(cdr rect))
(define (calc-perimeter rect)
  (let ((up (up-bound rect)) (down (down-bound rect)))
    (* 2 (+ (abs(- (x-point (start-segment up)) (x-point (end-segment up))))  (abs (- (y-point (start-segment up)) (y-point (start-segment down)))))
   )))
(define (calc-area rect)
  (let ((up (up-bound rect)) (down (down-bound rect)))
    (* (abs(- (x-point (start-segment up)) (x-point (end-segment up))))  (abs (- (y-point (start-segment up)) (y-point (start-segment down)))))
   )
  )
(define rect1  (make-rect (make-segment (make-point 0 0)(make-point 20 0) )  (make-segment (make-point 0 10)(make-point 20 10) ) ))

  
  (define (car z)            
   (if (= (gcd z 2) 1)      
     0                      
     (+ 1 (car (/ z 2))))) 
  

; exercise 2.6