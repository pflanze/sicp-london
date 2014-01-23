;; smoothing

(define dx 0.01)

(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
	  (f (- x dx))
	  (f (+ x dx)))
       3)))


(define f square)

(define g (smooth square))

;; repeatedly  (couple steps; heh~, vs different dx? wl digital, wlnow?)

(define h (smooth g))

;; hm n times?

;; hm efficiently? compose ?

;;(define ())

;;

;; (compose smooth f)
;; (smooth f)




