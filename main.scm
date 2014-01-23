(define (filter fn xs)
    (cond ((null? xs) xs)
	  (else
	   (if (fn (car xs))
	       (cons (car xs) (filter fn (cdr xs)))
	       (filter fn (cdr xs))))))

(define (same-parity x . xs)
  (filter (if (even? x) even? odd?)
	  xs))
