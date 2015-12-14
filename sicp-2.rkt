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



; exercise 2.6
(define zero (lambda (f)(lambda (x) x)))
(define one (lambda (f)(lambda (x) (f x))))
(define two (lambda (f)(lambda (x) (f (f x)))))
(define (add-1 n) (lambda (f)(lambda (x) (f((n f) x)))))
(define (add a b)(lambda (f)(lambda (x) ((a f)((b f) x)))))

; exercise 2.7
(define (make-interval a b)(cons a b))
(define (upper-bound interval)(max (car interval)(cdr interval)))
(define (lower-bound interval)(min (car interval)(cdr interval)))
 (define (add-interval x y) 
   (make-interval (+ (lower-bound x) (lower-bound y)) 
                  (+ (upper-bound x) (upper-bound y))))
; exercise 2.8
(define (sub-interval x y) 
   (add-interval x (make-interval (-(upper-bound y)) (-(lower-bound y)) )))

; exercise 2.9
(define (equal? a b)(= a b))
(define (get-width interval)(/ (- (upper-bound interval) (lower-bound interval)) 2))
(define (interval-equal? in1 in2 operation)(equal?  (abs(+ (get-width in1) (get-width in2)))  (get-width (operation in1 in2))))
(define in1 (make-interval 2 5))
(define in2 (make-interval 4 10))

(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
(p2 (* (lower-bound x) (upper-bound y)))
(p3 (* (upper-bound x) (lower-bound y)))
(p4 (* (upper-bound x) (upper-bound y))))
(make-interval (min p1 p2 p3 p4)
(max p1 p2 p3 p4))))

; exercise 2.10
(define (div-interval x y)
  (if (> (* (upper-bound y) (lower-bound y)) 0)
  (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y))))
  (error "Division error (interval spans 0)" y) 
  ))


; exercise 2.12
(define (make-center-percent center percent)(make-interval (* center (- 1 (/ percent 100))) (* center (+ 1 (/ percent 100)))))
(define (percent i)(* 100 (/ (- (upper-bound i) (center i)) (center i))))
(define (center i)(/ (+ (lower-bound i) (upper-bound i)) 2))

(define a (cons (list 1 2) (list 3 4)))
(define b (cons a a))

; exercise 2.17
(define (last-pair l)
  (let ((next (cdr l))) (if  (null? next) l (last-pair next)))
  )

; exercise 2.18
(define (reverse l)
  (define (reverse_iter res l)
       (if (null? l) res (reverse_iter (cons (car l) res) (cdr l)))
    )
  (reverse_iter nil l)
  )

; exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)
            )
         )
        )
  )
(define nil '())
(define (first-denomination list) (car list))
(define (except-first-denomination list) (cdr list))
(define (no-more? list)(null? list ))
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))



; exercise 2.20
(define (same-parity . l)
  (let ((re (remainder (car l) 2)))
  (define (test? n)(= re (remainder n 2)))
  (define (iter res ls)
    (if (null? ls) (reverse res) (iter (if (test? (car ls)) (cons (car ls) res) res) (cdr ls)))
    )
  (iter nil l))
  )

(define (len list)(if (null? list) 0 (+ 1 (len (cdr list)))))
(define (avg . l)
  (define (iter res ls)
    (if (null? ls) (/ res (len l)) (iter (+ res (car ls)) (cdr ls)))
    )
  (iter 0 l)
  )

; exercise 2.21
(define (square-list items)
(if (null? items)
nil
(cons (square (car items)) (square-list (cdr items)))))
(define (square-list2 items)
(map square items))


; exercise 2.23
(define (for-each nothing pro list)
 (if (null? list) (newline) (for-each (pro (car list)) pro (cdr list)))
  )


(define (for-each2 f items)
  (if (null? items)
      (newline)
      (let ()
        (f (car items))
        (for-each2 f (cdr items)))))

(define v1 (list 1 2 3))
(define v2 (list 4 5 6))
(define (append list1 list2)
(if (null? list1)
list2
(cons (car list1) (append (cdr list1) list2))))


; exercise 2.27
(define (deep-reverse l)
  (define (reverse_iter res l)
    (print res)
    (print l)
    (newline)
       (if (null? l) res (reverse_iter (cons (if (pair? (car l)) (deep-reverse (car l)) (car l)) res) (cdr l)))
    )
  (reverse_iter nil l)
  )

; exercise 2.28
(define (fringe l)
  ; select/delete the first non-list element 
  (define (selector l) (if (and (list? l)(not (null? l)))
                           (if (null? (car l))(selector (cdr l)) (selector (car l)))
                           l
                           ))  
 (define (deletor l) (if (and (list? l)(not (null? l)))
                          (if (list? (car l))
                           (if (null? (car l))  (deletor (cdr l)) (cons (deletor (car l)) (cdr l)) )
                           (cdr l)
                           )
                          nil
   )) 

 (define (fringe_iter res l)
       (if (null? l)
           (reverse res)
           (if (null? (selector l))
               (fringe_iter res (deletor l))
               (fringe_iter (cons (selector l) res) (deletor l)))
           )
    )
  (fringe_iter nil l)
  )

(define v (list (list 1 2) (list 3 4)))




(define (print x)
  (newline)
  (display x))


; exercise 2.29
; ?????????????????



