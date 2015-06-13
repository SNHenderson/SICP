#lang scheme

(define (sum term a next b)
	;(display a) (newline)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))
	)
)

(define (simp-integral f a b n)
	(define (coefficient-f x)
		(cond 
			((or (= x 0) (>= (/ x (h))  n) ) 
				(f x)
			)
		    (else
				(* (+ (* 2 (modulo (inexact->exact (round (/ x (h)) ) ) 2) ) 2) (f x))
			)
		)
	)
	(define (h) 
		(/ (- b a) n)
	)
	(define (next-yk x) 
		(+ x (h))
	)
	(* (/ (h) 3) (sum coefficient-f a next-yk b) )
)

(define (cube a) 
	(* a a a)
)

(define pi 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899)

(simp-integral cube 1 2 10000)
