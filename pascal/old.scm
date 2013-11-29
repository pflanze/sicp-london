;; This version uses lists, but it has better orders of growth.


(define first car)
(define rest cdr)
(define second cadr)

;; (1)

(define (pascal-row l)
  (define (subpascal l)
    (if (or (null? l)
	    (null? (rest l)))
	(list) ;; '()
	(cons (+ (first l) (second l))
	      (subpascal (rest l)))))
  (append (list 1) (subpascal l) (list 1)))

(define (pascal* n l)
  (if (zero? n)
      l
      (pascal* (- n 1) (pascal-row l))))

(define (pascal n p)
  (list-ref (pascal* n '(1)) p))

