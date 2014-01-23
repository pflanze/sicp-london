(define (filter fn xs)
    (cond ((null? xs) xs)
	  (else
	   (if (fn (car xs))
	       (cons (car xs) (filter fn (cdr xs)))
	       (filter fn (cdr xs))))))


;; (define (odd? x)
;;   (if (= x 1) #t
;;       (even? (- x 1))))
;; (define (even? x)
;;   (if (= x 0) #t
;;       (odd? (- x 1))))

(define (even? x)
  (integer? (/ x 2)))

(define (odd? x)
  (not (even? x)))

(define (same-parity x . xs)
  (filter (if (even? x)
	      even?
	      odd?)
	  xs))
