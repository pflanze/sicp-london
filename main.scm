(define (rev l)
  (if (null? l)
      l
      (append (rev (cdr l)) (list (car l)))))

