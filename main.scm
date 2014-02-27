
;; d (u^n)
;; ------ =  nu^ (n-1) * ( du / dx)
;;  dx


;; ** for expt

(define ex '(** (* x x) 2))
(define examples
  '(x
    (** (* x x) 2)))


(define (der e by)
  (let ((terms
	 (list
	  (cons '*
		(lambda (x1 x2)
		  (list '+
			(list '* x2 (der x1 by))
			(list '* x1 (der x2 by)))))
	  (cons '**
		(lambda (u n)
		  (list '*
			n
			(list '** u (dec n));;XX
			(der u by)))))))
    
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
 (* 4 (** x 3)))


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
	 (define fd
	   (eval (list 'lambda (list by)
		       (deriv e by))))
	 (let ((dn (numder f testx))
	       (ds (fd testx)))
	   (list dn ds)))
       testnumbers))

;; (TEST
;;  > ())




(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product p1 p2)
  (cond ((or (=number? p1 0) (=number? p2 0)) 0)
	((=number? p1 1) p2)
	((=number? p2 1) p1)
	((and (number? p1) (number? p2)) (* p1 p2))
	(else (list '* p1 p2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
				 (multiplicand exp))))))

(define (expon? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (expon-base exp) (cadr exp))

(define (expon-pow exp) (caddr exp))

(define (make-expon base pow)
  (cond ((not (number? pow))
	 (error "power must be a number!"))
	((=number? pow 1) base)
	((=number? pow 0) 1)
	(else (list '** base pow))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var) (multiplicand exp))))
	((expon? exp)
	 (make-product (expon-pow exp)
		       (make-product (make-expon (expon-base exp)
						 (- (expon-pow exp) 1))
				     (deriv (expon-base exp) var))))))
