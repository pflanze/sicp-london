(define make-hash make-table)
(define hash-set! table-set!)
(define (hash-ref t k)
  (table-ref t k #f))

(define (square x) (* x x))

;; (define (make-from-real-imag x y)
;;   (define (dispatch op)
;;     (cond ((eq? op 'real-part) x)
;;           ((eq? op 'imag-part) y)
;;           ((eq? op 'magnitude)
;;            (sqrt (+ (square x) (square y))))
;;           ((eq? op 'angle) (atan y x))
;;           (else
;;            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
;;   dispatch)
  
  
(define +optable+ (make-hash))
(define (put op type proc)
  (hash-set! +optable+ (list op type) proc))
(define (get op type)
  (hash-ref +optable+ (list op type)))

(define +coercion-table+ (make-hash))
(define (put-coercion type1 type2 proc)
  (hash-set! +coercion-table+ (list type1 type2) proc))
(define (get-coercion type1 type2) 
  (hash-ref +coercion-table+ (list type1 type2)))



(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum)
	 (car datum))
	((number? datum)
	 'scheme-number)
	(else
	 (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum)
	 (cdr datum))
	((number? datum) datum)
	(else
	 (error "Bad tagged datum -- CONTENTS" datum))))

(define (on-contents-1 fn)
  (lambda (x)
    (fn (contents x))))
(define (on-contents-2 fn)
  (lambda (x y)
    (fn (contents x) (contents y))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc args)
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)


(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car (contents z)))
  (define (imag-part z) (cdr (contents z)))
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
  (put 'equ? '(rectangular rectangular)
       (lambda (x y)
	 (and (= (real-part x) (real-part y))
	      (= (imag-part x) (imag-part y)))))
  (put 'zero? '(rectangular)
       (lambda (x)
	 (and (zero? (real-part x))
	      (zero? (imag-part x)))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car (contents z)))
  (define (angle z) (cdr (contents z)))
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
  (put 'equ? '(polar polar)
       (lambda (x y)
	 (or (and (zero? (magnitude x))
		  (zero? (magnitude y)))
	     (and (= (magnitude x) (magnitude y))
		  (= (angle x) (angle y))))))
  (put 'zero? '(polar)
       (lambda (x)
	 (zero? (magnitude x))))
  'done)



(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic 'zero? x))
 

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (mixed-equ? x y)
    (and (= (real-part x) (real-part y))
	 (= (imag-part x) (imag-part y))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;; and delegates:
  (put 'magnitude '(complex) (on-contents-1 magnitude))
  (put 'angle '(complex) (on-contents-1 angle))
  (put 'real-part '(complex) (on-contents-1 real-part))
  (put 'imag-part '(complex) (on-contents-1 imag-part))
  (put 'equ? '(complex complex) (on-contents-2 equ?))
  (put 'equ? '(polar rectangular) mixed-equ?)
  (put 'equ? '(rectangular polar) mixed-equ?)
  (put 'zero? '(complex) (on-contents-1 =zero?))
  'done)

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make '(scheme-number)
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number) =)
  (put 'zero? '(scheme-number) zero?)
  'done)



(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

  
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

(define louis-reasoner-z (make-complex-from-real-imag 3 4))


(TEST
 > (mul 5 3)
 15
 > (mul 2 (mul 5 3))
 30)

(TEST
 > (=zero? 4)
 #f
 > (=zero? 0)
 #t
 > (=zero? (make-complex-from-real-imag 4 9))
 #f
 > (=zero? (make-complex-from-real-imag 0 9))
 #f
 > (=zero? (make-complex-from-real-imag 9 0))
 #f
 > (=zero? (make-complex-from-real-imag 0 0))
 #t
 > (=zero? (make-complex-from-mag-ang 0 0))
 #t
 > (=zero? (make-complex-from-mag-ang 4 0))
 #f
 > (=zero? (make-complex-from-mag-ang 0 4))
 #t
 > (equ? (make-complex-from-mag-ang 0 4) (make-complex-from-mag-ang 0 5))
 #t
 > (equ? (make-complex-from-mag-ang 0 4) (make-complex-from-mag-ang 5 0))
 #f
 ;; mixed:
 > (equ? (make-complex-from-mag-ang 0 4) (make-complex-from-real-imag 0 0))
 #t
 )


(define (exp x y) (apply-generic 'exp x y))
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)


(add (make-scheme-number 5) louis-reasoner-z)

