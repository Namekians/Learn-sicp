#lang racket
(define x (cons 3 4))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat a b)(cons a b))
(define (numer x)(car x))
(define (denom x)(cdr x))

(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))