
;; (        (1)  row 0
;;         (1 1) row 1
;;        (1 2 1) row 2
;;       (1 3 3 1) row 3
;;     (1 4 6 4 1) row 4
;;    (1 5 10 10 5 1) row 5
;;   (1 6 15 20 15 6 1)
;;      1 2
;;  (1 7 21 35 35 21 7 1))
;;       2
(define (pascal n p)
  (if (zero? p)
      1
      (if (zero? n)
	  0
	  (+ (pascal (- n 1) (- p 1))
	     (pascal (- n 1) p)))))
