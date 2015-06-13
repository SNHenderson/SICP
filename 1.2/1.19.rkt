#lang scheme

(define (fib n)
  (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
    (cond 
        ((= count 0) 
            b
        )

        ((even? count)
            (fib-iter a b (+ (* p p) (* q q)) (+ (* 2 p q) (* q q)) (/ count 2))
        )

        (else 
            (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1))
        )
    )
)

(fib 10000)

; p' and q':
; on 2T, a = (b p q + a q^2) + (b q^2 + a q^2 + a p q) + (b q p + a q p + a p^2 ) and b = (b p^2 + a q p) + (b q^2 + a q^2 + a p q) 
; Expanded form: a = bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2 and b = bp^2 + aqp + bq^2 + aq^2 + aqp
; Grouping like forms: a = (2bpq + bq^2) + (2apq + 2aq^2 + ap^2) and b = (bp^2 + bq^2) + (2aqp + aq^2)
; Factor out 'a's and 'b's: a = b(2pq + q^2) + a(2pq + 2q^2 + p^2) and b = b(p^2 + q^2) + a(2qp + q^2)
; Modify a = to fit "b q + a q + a p" format: a = b(2pq + q^2) + a(2pq + q^2) + a(q^2 + p^2) 
; q' = (2pq + q^2)
; p' = (q^2 + p^2) 
