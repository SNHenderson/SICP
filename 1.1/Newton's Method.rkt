#lang scheme

(define (^2 x) (* x x))

(define change 0)

(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)))

(define (improve guess x)
	(average guess (/ x guess)))

(define (average x y) 
	(/ (+ x y) 2))

(define (good-enough? guess x)
	(< (abs (- guess (improve guess x) ) ) 1/10000000 ) ) 

(define (sqrt x)
	(sqrt-iter 1.0 x))

(sqrt 99999999)