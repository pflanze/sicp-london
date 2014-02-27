
;; d (u^n)
;; ------ =  nu^ (n-1) * ( du / dx)
;;  dx


;; ** for expt

(define ex '(** (* x x) 2))


(define (der e by)
  (let ((terms
	 (list
	  (cons '**
		(lambda (u x)
		  (list '* x
			(list '** u
			      ;; (list '- x 1)
			      (dec x))))))))
    
    (cond ((pair? e)
	   (let ((a (car e)))
	     (if (symbol? a)
		 (cond ((assq a terms)
			=> (lambda (k.expander)
			     (apply (cdr k.expander) (cdr e))))
		       (else
			(error "no function definitions?")))
		 (error "no first class functions?"))))
	  ((symbol? e)
	   (if (eq? e by)
	       1
	       (error "unfinished:  e or 0?")))
	  ((number? e)
	   0)
	  (else
	   (error "unhandled:" e)))))

(TEST
 > (der ex)
 (* 2 (** (* x x) 1)))


;; numeric differentiation
(define dx 0.0001)
(define (numder f x)
  (/ (- (f (+ x dx)) (f x))
     dx))

(define testnumbers
  (list -100 -10 -4 -.2 -.1 0 .1 .4 1.3 5 6 9 10 89))


(define ** expt)

(define (check e by)
  (define f (eval (list 'lambda (list by) e)))
  (map (lambda (testx)
	 (define fd (eval (list 'lambda (list by) (der e by))))
	 (let ((dn (numder f testx))
	       (ds (fd testx)))
	   (list dn ds)))
       testnumbers))

;; (TEST
;;  > ())