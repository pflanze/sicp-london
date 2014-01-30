(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car seq)
	  (accumulate op initial (cdr sequence)))))

