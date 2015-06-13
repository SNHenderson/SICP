#lang scheme

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) 
		   (sum term (next a) next b))))

(define (simp-integral f a b n)
	(* 	(/ (/ (- b a) n) 3) 
	(sum (lambda (x) (cond ((or (= x 0) (= x n) ) (f x)) (else (* (+ (* 2 (modulo (/ x (/ (- b a) n)) 2)) 2) (f x))))) a (lambda (x) (+ x (/ (- b a) n))) b)))

(define (cube a) (* a a a))

(define (fifth a) (* a a a a a))

(simp-integral (lambda (x) (sqrt (sin x))) 0 10 10)
