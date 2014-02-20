(define (id cont v)
  (cont v))

(define (main x)
  (display x) (newline))

(define (t)
  (id main 1))


