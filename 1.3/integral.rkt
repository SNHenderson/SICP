#lang scheme

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) 
		   (sum term (next a) next b))))

(define (simp-integral f a b n)
	(define (coefficient-f x)
		 (cond ((or (= x 0) (>= (/ x (h))  n) ) 
				(f x))
		    (else	
				(* (+ (* 2 (modulo (/ x (h)) 2)) 2) (f x)))))
	(define (h) (/ (- b a) n))
	(define (next-yk x) (+ x (h)))
	(* (/ (h) 3) (sum coefficient-f a next-yk b)))

(define (cube a) (* a a a))

(define (fifth a) (* a a a a a))

(simp-integral cube 0 10 10)
