#lang scheme

(define (^2 x) (* x x))

(define (cube-root x) ;Cube root function
	(define (cube-iter guess) ;Approximate value for Cube root of x
		(if (good-enough? guess) 
			guess
			(cube-iter (improve guess)))) 
	(define (good-enough? guess) 
		(< (abs (- guess (improve guess) ) ) 1/10000000 ) ) 
	(define (improve guess)
		(/ (+ (/ x (^2 guess) ) (* 2 guess) ) 3 ) )
	(cube-iter 1.0))

(cube-root 64)