(define (square x)
  (* x x))

;; 2 3  -> 8

;; 3 3  -> 27

(define (exp b n)
  
  (define (exp-help x nmults n)
    (cond ((= n 0)
	   x)

	  ((= n 1)
	   (* x b))

	  ((> (* nmults 2) n)
	   (exp-help x 1 (- n nmults)))

	  (else
	   (exp-help (square x) (* nmults 2) n))))

  (exp-help b 1 n))

