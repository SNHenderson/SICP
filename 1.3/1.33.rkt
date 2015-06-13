#lang scheme

(define (filtered-accumulate filter? combiner null-value term a next b)
	(if (> a b)
		null-value
		(if (filter? a)
		    (combiner (term a) (filtered-accumulate filter? combiner null-value term (next a) next b))
		    (combiner null-value (filtered-accumulate filter? combiner null-value term (next a) next b))
		)
	)
)

(define (square x)
	(* x x)
)

(define (smallest-divisor n)
	(find-divisor n 2)
)

(define (next n)
	(if (= n 2)
		3
		(+ n 2)
	)
)

(define (find-divisor n test-divisor)
	(cond
		((> (square test-divisor) n) 
        	n
     	)

        ((divides? test-divisor n) 
        	test-divisor
        )

        (else 
        	( find-divisor n (next test-divisor) )
    	)
    )
)

(define (divides? a b)
	(= (remainder b a) 0))

(define (prime? n)
	(and (> n 1) (= n (smallest-divisor n)))
)

(define (inc n)
	(+ n 1)
)

(define (sum-prime-square a b)
	(filtered-accumulate prime? + 0 square a inc b)
)

(define (gcd a b)
	(cond ((= b 0) a)
		(else (gcd b (modulo a b)))
	)
)

(define (identity n)
	n
)

(define (product-relative-primes n)
	(define (relatively-prime? i)
		(= (gcd i n) 1)
	)
	(filtered-accumulate relatively-prime? * 1 identity 1 inc n)
)

(sum-prime-square 1 10)
(product-relative-primes 10)