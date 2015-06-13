#lang scheme

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) 
		   (sum term (next a) next b))))

(define (expt b n)
	(define (iter-fast-expt a b n) ; This is intentionally designed to be three variables so that iteration can be tested.
		( cond 
			((= n 0) 
				a 
			)

			((even? n) 
				(iter-fast-expt a (square b) (/ n 2))
			)

			(else 
        	(iter-fast-expt (* a b) b (- n 1))
    		)
    	)
	)
	(iter-fast-expt 1 b n)
)

(define (derivative f a b n)
	(define (coefficient-f x)
		(* (/ x (h)) x (expt x)



		 (cond ((or (= x 0) (= x n) ) 
				(f x))
		    (else	
				(* (+ (* 2 (modulo (/ x (h)) 2)) 2) (f x)))))
	(define (h) (/ (- b a) n))
	(define (next-yk x) (+ x (h)))
	(* (/ (h) 3) (sum coefficient-f a next-yk b)))

(define (cube a) (* a a a))

(define (fifth a) (* a a a a a))

(derivative cube 0 10 10)
