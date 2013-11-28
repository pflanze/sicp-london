(define (square x)
  (* x x))

;; 2 3  -> 8

;; 3 3  -> 27

(define (exp b n)
  (exp-help b 0 n))

(define (exp-help x nmults n)
  (if (> (* nmults 2) n)
      (exp-help x (- n nmults) n)
      (exp-help (square x) (* nmults 2) n)))

