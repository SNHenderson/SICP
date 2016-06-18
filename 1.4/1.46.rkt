#lang scheme

(define (iterative-improve good-enough? improve)
	(lambda (x)
		(if (good-enough? x)
			x
			(improve x))))

(define (sqrt-good x y)
	(= (* x x) y))