#lang scheme 

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next)
			)
		)
	)
	(try first-guess)
)


(define (cont-frac n d k)
	(define (frac i)
		(if (= i k)
		    (/ (n i) (d i))
		    (/ (n i) (+ (d i) (frac (+ i 1))))
		)
	)
	(frac 1)
)

(define (iter-cont-frac n d k)
	(define (iter i result)
		(if (= i 0)
			result
			(iter (- i 1) (/ (n i) (+ (d i) result)) )
		)
	)
	(iter k 0)
)


(define (golden-ratio)
	(fixed-point 
	(lambda (x) (+ 1 (/ 1 x)))
	1.0)
)

(display "fixed-point golden-ratio: ")
(/ 1 (golden-ratio))

(display "cont-frac golden-ratio: ")
(cont-frac (lambda (i) 1.0)
       (lambda (i) 1.0)
       11)

(display "iter-cont-frac golden-ratio: ")
(iter-cont-frac (lambda (i) 1.0)
       (lambda (i) 1.0)
       11)
(newline)