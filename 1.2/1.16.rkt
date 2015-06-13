#lang scheme

; Exercise 1.16: Design a procedure that evolves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps,
; as does fast-expt. (Hint: Using the observation that ( b^(n / 2) )^2 = ( b^2 )^(n / 2) , keep, along with the exponent n and the base b , 
; an additional state variable a , and define the state transformation in such a way that the product a*b^n is unchanged from state to state.
; At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process.
; In general, the technique of defining an invariant quantity that remains unchanged from state to state
; is a powerful way to think about the design of iterative algorithms.) 

; b^n = (b^(n / 2))^2 if n is even
; b^n = b * b^(n − 1) if n is odd

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (expt b n)
  (if (= n 0) 
      1 
      (* b (expt b (- n 1)))))

(define (fast-expt b n)
	( cond 
		((= n 0) 
			1 
		)

		((even? n) 
			(square (fast-expt b (/ n 2))) 
		)

		(else 
         (* b (fast-expt b (- n 1)))
    	)
    )
)

(define (even? n)
	(= (remainder n 2) 0)
)

(define (square x)
  (* x x)
)

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

(define (iter-fast-expt-debug a b n) ; Must absolutely copy iter-fast-expt in every way except for display statements
	( cond 
		((= n 0) 
			a 
		)

		((even? n) 
			(display (* a (fast-expt b n))) ;debug display, good for visualizing how it works
			(display ", a = ")(display a)(display ", b = ")(display b)(display ", n = ")(display n)(newline)			
			(iter-fast-expt-debug a (square b) (/ n 2))
		)

		(else
			(display (* a (fast-expt b n))) ;debug display, good for visualizing how it works
			(display ", a = ")(display a)(display ", b = ")(display b)(display ", n = ")(display n)(newline)		 
        	(iter-fast-expt-debug (* a b) b (- n 1))
    	)
    )
)

;(time (expt 100000 100000))
;(time (expt-iter 100000 100000 1))
;(time (fast-expt 100000 100000))
(iter-fast-expt 1 3 3)(newline)
(iter-fast-expt-debug 1 3 2)(newline)
(iter-fast-expt-debug 81 6561 12) ;Iterative proof

;			(display (* a (fast-expt b n))) ;debug display, good for visualizing how it works
;			(display ", a = ")(display a)(display ", b = ")(display b)(display ", n = ")(display n)(newline)	