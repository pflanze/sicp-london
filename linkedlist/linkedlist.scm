(define linked-list
  (lambda (a r)
    (lambda (b)
      (if b a r))))

(define get-data
  (lambda (l)
    (l #t)))

(define get-pointer
  (lambda (l)
    (l #f)))

