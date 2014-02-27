
;; d (u^n)
;; ------ =  nu^ (n-1) * ( du / dx)
;;  dx


;; ** for expt

(define ex '(** (* x x) 2))


(define (der e)
  (let ((terms
	 (list
	  (cons '**
		(lambda (u x)
		  (list '* x
			(list '** u
			      ;; (list '- x 1)
			      (dec x))))))))
    
    (if (pair? e)
	(let ((a (car e)))
	  (if (symbol? a)
	      (cond ((assq a terms)
		     => (lambda (k.expander)
			  (apply (cdr k.expander) (cdr e))))
		    (else
		     (error "no function definitions?")))
	      (error "no first class functions?")))
	;; assume constant?
	e)))

(TEST
 > (der ex)
 (* 2 (** (* x x) 1)))

