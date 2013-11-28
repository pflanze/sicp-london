(define (square x)
  (* x x))

;; 2 3  -> 8

;; 3 3  -> 27

;; (define (exp b n)
  
;;   (define (exp-help x nmults n)
;;     (cond ((= n 0)
;; 	   x)

;; 	  ((= n 1)
;; 	   (* x b))

;; 	  ((> (* nmults 2) n)
;; 	   (exp-help x 1 (- n nmults)))

;; 	  (else
;; 	   (exp-help (square x) (* nmults 2) n))))

;;   (exp-help b 1 n))


;; (define (exp a pow)
;;   (if (= pow 1)
;;       a
;;       (exp (* a a) ())))

(define (exp a b pow)
  (if (= pow 1) a
      (exp (* a b) b (- pow 1))))

