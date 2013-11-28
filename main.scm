(define (square x)
  (* x x))

;; 2 3  -> 8

;; 3 3  -> 27

(define (exp b n)
  (exp-help b 1 n))

(define (exp-help x nmults n)
  (cond ((> (* nmults 2) n)
	 (exp-help x (- n nmults) n))
	((= (* nmults 2) n)
	 )
	(else
	 (exp-help (square x) (* nmults 2) n))))

