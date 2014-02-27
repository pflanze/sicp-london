(define (cc x coins)
  (define (_cc x coins tot)
    (cond ((zero? x) (inc tot))
	  ((negative? x) tot)
	  ((null? coins) tot)
	  (else
	   ;; coins left, and amount to return
	   ()))))