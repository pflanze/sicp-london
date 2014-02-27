(define (square x)
  (* x x))

;; 2 3  -> 8

;; 3 3  -> 27

(define (exp b n)
  
  (define (exp-help x nmults n)
    (cond ((= n 0)
	   x)

	  ((= n 1)
	   (* x b))

	  ((> (* nmults 2) n)
	   (exp-help x 1 (- n nmults)))

	  (else
	   (exp-help (square x) (* nmults 2) n))))

  (exp-help b 1 n))

;; > (exp 4 16) 
;; 4294967296


;; demonstrated O(n) versions, recursive and iterative:

;; (define (exp a pow)
;;   (if (= pow 1)
;;       a
;;       (* a (exp a (- pow 1)))))

;; > (exp 4 16) 
;; 4294967296

;; (define (exp a b pow)
;;   (if (= pow 1) a
;;       (exp (* a b) b (- pow 1))))

;; > (exp 1 4 17) 
;; 4294967296
