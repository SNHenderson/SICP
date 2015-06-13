#lang scheme

(define (runtime) (current-milliseconds))

(define (square x)
  (* x x)
)

(define (even? n)
	(= (remainder n 2) 0)
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
	(= n (smallest-divisor n))
)

(define (timed-prime-test n)
	(start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
	(cond 
		((prime? n)
			(report-prime (- (runtime) start-time) n)
		)
	)
)

(define (report-prime elapsed-time n)
	(newline)	
	(display n)
	(display " *** ")
	(display elapsed-time)
)

(define (search-for-primes a b)
	(define (start-prime-search a b start-time)
  		(cond 
  			((even? a) 
  				(start-prime-search (+ a 1) b start-time)
  			)

  			((< a b) 
  				(cond 
  					((prime? a)
  						(newline)
  						(display a)
  					)
  				)
  				(start-prime-search (+ a 2) b start-time)
  			)

  			(else 
  				(displayRuntime (- (runtime) start-time))
  			)
  		)
  	)
  	(define (displayRuntime time)
  	 	(newline)
  	 	(display "		")
  		(display "Runtime: ")
  		(display time)
  		(newline)
  	)
  	(start-prime-search a b (runtime))
)

(time (search-for-primes 0 9999999))