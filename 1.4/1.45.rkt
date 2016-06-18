#lang scheme

(define (square x)
	(* x x))

(define (fourth x)
	(* x x x x))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
	(lambda (x) 
		(average x (f x))))

(define (compose f g)
	(lambda (x) (f (g x))))

(define (repeated f n)
	(if (= n 1)
		f
		(repeated (compose f f) (- n 1))))

(define (nth-root x n)
	(fixed-point 
		((repeated average-damp (floor (log2 n)))
			(lambda (y) 
				(/ x (^ y (- n 1)))))
		1.0))

(define (^ x n)
	(if (= n 1)
	    x
	    (* x (^ x (- n 1)))))

(define (log2 x)
	(/ (log x) (log 2)))

(nth-root 16 2)

