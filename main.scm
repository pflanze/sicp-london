(define (flatmap fn lis)
  (fold-right (lambda (v r)
		(append (fn v) r))
	      '()
	      lis))

(define (enumerate-interval start end)
  (iota (inc (- end start)) start))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
	   (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; row col
;; ((1 2) ..)

(define empty-board '())

(define (adjoin-position row col rest-of-queens )
  (cons (list row col) rest-of-queens))

(define board-size 8);; XX

(define (safe? k positions)
  ;; check that none of the columns is the same?
  (let ((kpositions (filter (lambda (position)
			      (= k (car (cdr position))))
			    positions)))
    (and (= (length kpositions) 1)
	 (let* ((newpos (car kpositions))
		(others (filter (lambda (x)
				  (not (equal? x newpos)))
				positions)))
	   (let ((row (car newpos)))
	     (and (null? (filter (lambda (pos)
				   (= row (car pos)))
				 others))
		  ;; diagonal
		  ç))))))

;; empty-board

;; safe?

;; adjoin-position

;; rest-of-queen

