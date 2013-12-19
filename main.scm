(define (square x)
  (* x x))

(define (accumulate combiner null-value term a next b)
  (define (acc tot x)
    (if (< x b)
	(acc (combiner tot (term x)) (next x))
	tot))
  (acc null-value a))


;; product of the square root of every other number between a and b

(define (prodsqother a b)
  (accumulate * 1 sqrt a (lambda (x) (+ x 2)) b))

