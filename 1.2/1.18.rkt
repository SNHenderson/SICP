#lang scheme

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (even? n)
	(= (remainder n 2) 0)
)

(define (mult a b)
	( cond 
		((= b 0) 
			0
		)		

		((even? b) 
			(mult (double a) (halve b))
		)

		(else 
        	(+ a (mult a (+ b -1)))
    	)
    )	
)

(define (* a b n) ; This is intentionally designed to be three variables so that iteration can be tested.
	;(display (+ n (mult a b))) ;debug display, good for visualizing how it works
	;(display ", a = ")(display a)(display ", b = ")(display b)(display ", n = ")(display n)(newline)
	( cond 
		((= b 0) 
			n
		)		

		((even? b) 
			(* (double a) (halve b) n)
		)

		(else 
        	(* a (+ b -1) (+ n a))
    	)
    )	
)

(* 10 120 0) 
; Debug output:
;1200, a = 10, b = 120, n = 0 ; While b is even, the equation we are using is (a*b) = (2a * b/2). 
;1200, a = 20, b = 60, n = 0
;1200, a = 40, b = 30, n = 0
;1200, a = 80, b = 15, n = 0 ;n stays the same. Right now, we're shrinking b and expanding a. 
;1200, a = 80, b = 14, n = 80 ;now b is odd, so the equation we are using is a*b = a+(b*b-1). Instead of adding a in the function and making it recursive, we add it to n.
	;Here's a visualization:
	;0 + 10*120 = 
	;0 + 20*60 =
	;0 + 40*30 =
	;0 + 80*15 =
	;Now, (80*15) = 80 + (80*14). Instead of doing (+ a (* a (+ b -1))), we go ahead and add a to n. 
	; 80*14 =
	; 160*7 = 
	; 160 + 160 * 6 (add 160 to n)
	; 160 * 6 =
	; 320 * 3 =
	; 320 + 320 * 2 (add 320 to n)
	; 320 * 2 =
	; 640 * 1 =
	; 640 + 640 * 0 (add 640 to n)
	; So, n = 80 + 160 + 320 + 640 + 0. Pretty much what this is doing is simplifying the process when b is even, and adding the result when b is odd. 
;debug continued:
;1200, a = 160, b = 7, n = 80
;1200, a = 160, b = 6, n = 240
;1200, a = 320, b = 3, n = 240
;1200, a = 320, b = 2, n = 560
;1200, a = 640, b = 1, n = 560
;1200, a = 640, b = 0, n = 1200
;1200
(* 320 2 560) ; Iterative proof
(* 120 10 0)
; Debug output:
;1200, a = 120, b = 10, n = 0
;1200, a = 240, b = 5, n = 0
;1200, a = 240, b = 4, n = 240
;1200, a = 480, b = 2, n = 240
;1200, a = 960, b = 1, n = 240
;1200, a = 960, b = 0, n = 1200
;1200

;( in the above version, n = 240 + 960. Since what * does is increase a and decrease b, a should always be the larger number)

; Demonstration: (Multiplying 10*100 has four more calls than multiplying 10000 * 10)

(* 10 100 0)
;1000, a = 10, b = 100, n = 0
;1000, a = 20, b = 50, n = 0
;1000, a = 40, b = 25, n = 0
;1000, a = 40, b = 24, n = 40
;1000, a = 80, b = 12, n = 40
;1000, a = 160, b = 6, n = 40
;1000, a = 320, b = 3, n = 40
;1000, a = 320, b = 2, n = 360
;1000, a = 640, b = 1, n = 360
;1000, a = 640, b = 0, n = 1000
;1000
(* 10000 10 0)
;100000, a = 10000, b = 10, n = 0
;100000, a = 20000, b = 5, n = 0
;100000, a = 20000, b = 4, n = 20000
;100000, a = 40000, b = 2, n = 20000
;100000, a = 80000, b = 1, n = 20000
;100000, a = 80000, b = 0, n = 100000
;100000