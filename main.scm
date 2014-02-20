(define (id cont v)
  (cont v))

(define (main x)
  (display x) (newline))

(define (t)
  (id main 1))


;; (define (factorial cc n)
;;   (if (= n 0) ;;wl
;;       (cc 1)
;;       ;; fact (n+1) + fact (n+2)  gl
;;       (factorial (lambda (cc n)
;; 		   (factorial cc (- n 2)))
;; 		 (- n 1))))

;;wrong fib depp

(define (factorial cc n)
  (if (= n 0)
      (cc 1)
      (factorial (lambda (x)
		   (cc (* n x)))
		 (- n 1))))

