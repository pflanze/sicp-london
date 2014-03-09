;; "λf.(λx.f (x x)) (λx.f (x x))"

(define-macro* (λ bs . body)
  `(lambda ,bs ,@body))

(define D
  (λ (f)
     ((λ (x)
	 (f (x x)))
      (λ (x)
	 (f (x x))))))

;; blows the stack. and never calls f.

