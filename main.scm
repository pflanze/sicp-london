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


(define sc #f)
(define (save! c)
  (call/cc (lambda (cc)
	     (set! sc cc)
	     x)))


(define (current-continuation)
  (call/cc (L (cc) cc)))

(define (tt)
  (let ((cc (current-continuation)))
    ;; (cc cc)  dann
    (cc 0)))



;; label goto


(define label current-continuation)

(define (goto l)
  (l l))

(define (t3)
  (define c 0)
  (let ((l (label)))
    (println c)
    (set! c (+ c 1))
    (if (< c 10)
	(goto l))))


;; (define (t3)
;;   (define c 0)
;;   (lambda (XXX)
;;     (let ((l XXX))
;;       (println c)
;;       (set! c (+ c 1))
;;       (if (< c 10)
;; 	  (goto l)))))


(define (t4 yield)
  (define c 0)
  (let ((l (label)))
    (yield c)
    (set! c (+ c 1))
    (if (< c 10)
	(goto l))))

(define (use-t4.1)
  (t4 println))

;; (define (produce fn)
;;   (call/cc (L (c)
;; 	      )))

(define (use-t4.1)
  (produce t4))

