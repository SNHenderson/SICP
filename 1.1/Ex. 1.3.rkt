#lang scheme
(define a 3)
(define b 4)
(define c 5)
(define (square x) (* x x) )
(define (addLargest x y z)
  (if (and (< z x) (< z y)) (+ (square x) (square y)) null) 
  (if (and (< y x) (< y z)) (+ (square x) (square z)) (+ (square z) (square y))) )

(addLargest a b c)

