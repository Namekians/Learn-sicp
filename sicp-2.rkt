#lang racket
(define x (cons 3 4))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat a b)
  (let ((g ((if (< b 0)  - +)(abs (gcd a b)))))(cons (/ a g)(/ b g)))
)
  
(define (numer x)(car x))
(define (denom x)(cdr x))

(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))

(define (abs x)(cond ((> x 0) x)
                     ((< x 0)(- x))
                     (else 0)
                     ))


