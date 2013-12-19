(define (square x)
  (* x x))

(define (accumulate combiner null-value term a next b)
  (define (acc tot x)
    (if (< x b)
	(acc (combiner tot (term x)) (next x))
	tot))
  (acc null-value a))


