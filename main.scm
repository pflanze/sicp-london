
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


;; numeric differentiation
(define dx 0.0001)
(define (numder f x)
  (/ (- (f (+ x dx)) (f x))
     dx))

(define testnumbers
  (list -100 -10 -4 -.2 -.1 0 .1 .4 1.3 5 6 9 10 89))


(define ** expt)

(define (check e)
  (define f (eval (list 'lambda '(x) e)))
  (map (lambda (x)
	 (define fd (eval (list 'lambda '(x) (der e))))
	 (let ((dn (numder f x))
	       (ds (fd x)))
	   (list dn ds)))
       testnumbers))

;; (TEST
;;  > ())