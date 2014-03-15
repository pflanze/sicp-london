(define *optable* (make-table))

(define (put op type proc)
  (table-set! *optable* (list op type) proc))

(define (get op type)
  (table-ref *optable* (list op type)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))



(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(install-polar-package)
(install-rectangular-package)



;; 3.3.2 ?


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))





;; ex 2.73

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) exp ;; (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;; Write procedure for sum, product, exponents  + code to install them


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

(define (augend s) ;;; confusing name now
;;  (warn "augend:" s)
  (values;; pp-through
   (let ((rest (cddr s)))
     (if (null? (cdr rest))
	 (car rest)
	 (cons '+ rest)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) ;; p1
  (cadr p))

(define (multiplicand p) ;; p2
;;  (warn "multiplicand:" p)
  (let ((args (cddr p)))
    (if (null? (cdr args))
	(car args)
	(cons '* args))))

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



(define-macro* (define-method op+type+args . body)
  (assert* pair? op+type+args
	   (lambda-pair ((op+type args))
		   (mcase op+type
			  (`(`op `type)
			   `(put ',op ',type
				 (lambda ,args ,@body)))))))

;; (+ x 3) -> (+ 1 0)

(define-method ((deriv +) exp var)
  (make-sum (deriv (addend exp) var)
	    (deriv (augend exp) var)))

(define-method ((deriv *) exp var)
  (make-sum (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (deriv (multiplier exp) var)
			  (multiplicand exp))))

(define-method ((deriv **) exp var)
  (make-product (expon-pow exp)
		(make-product (make-expon (expon-base exp)
					  (- (expon-pow exp) 1))
			      (deriv (expon-base exp) var))))

(TEST
 > (deriv '(+ x 3) 'x)
 1
 > (deriv '(+ (* x x) 30) 'x)
 (+ x x)
 > (deriv '(+ (** x 2) 30) 'x)
 (* 2 x))

