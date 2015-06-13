#lang scheme

(define (product term a next b)
	(if (> a b)
		1
		(* (term a) (product term (next a) next b))
	)
)

(define (iter-product term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)) )
		)
	)
	(iter a 1)
)

(define (f n)
	(+ n (- 2 (modulo n 2)) )
)


(define (g n)
	(+ n (+ 1 (modulo n 2)) )
)

(define (inc n)
	(+ n 1)
)

(define (pi-approx n)
	(* 4 (/ (product f 1 inc n) (product g 1 inc n)))
)

(define (iter-pi-approx n)
	(* 4 (/ (iter-product f 1 inc n) (iter-product g 1 inc n)))
)

(time (exact->inexact (pi-approx 100000)))
(time (exact->inexact (iter-pi-approx 100000)))