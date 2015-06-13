#lang scheme

; f ( n ) = n if n < 3
; f ( n ) = f ( n − 1 ) + 2 f ( n − 2 ) + 3 f ( n − 3 ) if n ≥ 3 . 

(define (f n)
	(if (< n 3) 
		n 
		(+ (f(- n 1 ))
			(* 2 (f(- n 2 )))
			(* 3 (f(- n 3 )))
		)
	)
)

(define (iterative-f n)  
	(define (f-iter a b c counter)
		(if (> counter (- n 3) ) 
			a
			(f-iter (+ a (* 2 b) (* 3 c) ) a b (+ counter 1) )
		)
	)
	(if (< n 3)
		n
		(f-iter 2 1 0 0)
	)
)


;for(i >= 3; i++)
; f(n - 1) + 2 f(n - 2)

(f 4)
(iterative-f 4)

(time (f 30) )
(time (f 30) )