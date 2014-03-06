
;; book
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;; union-set
;; intersection-set

;; bonus: O(n)

(define empty-tree '())

(define t1
  (make-tree 5
	     (make-tree 4
			(make-tree 3 empty-tree empty-tree)
			empty-tree)
	     (make-tree 7
			empty-tree
			(make-tree 10
				   (make-tree 8 empty-tree empty-tree)
				   (make-tree 11 empty-tree empty-tree)))))


(TEST
 > (element-of-set? 5 t1)
 #t
 > (element-of-set? 4 t1)
 #t
 > (element-of-set? 3 t1)
 #t
 > (element-of-set? 2 t1)
 #f
 > (element-of-set? 7 t1)
 #t
 > (element-of-set? 10 t1)
 #t
 > (element-of-set? 8 t1)
 #t
 > (element-of-set? 9 t1)
 #f
 > (element-of-set? 11 t1 )
 #t
 )


;;book
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(TEST
 > (define t2 (list->tree '(1 4 5 9 38)))
 )


(define (union-sorted a b)
  (cond ((null? a)
	 b)
	((null? b)
	 a)
	(else
	 (cond ((= (car a) (car b))
		(cons (car a)
		      (union-sorted (cdr a) (cdr b))))
	       ((< (car a) (car b))
		(cons (car a)
		      (union-sorted (cdr a) b)))
	       (else
		(cons (car b)
		      (union-sorted a (cdr b))))))))

(TEST
 > (union-sorted (list 1 2 3 4 5) (list 2 4 6 7))
 (1 2 3 4 5 6 7))


(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; O(n) ? well

(define (union-tree.easy a b)
  (list->tree (union-sorted (tree->list a) (tree->list b))))

(define (union-tree a b)
  (cond ((null? a) b)
	((null? b) a)
	(else
	 (cond ((= (entry a) (entry b))
		(make-tree (entry a)
			   (union-tree (left-branch a)
				       (left-branch b))
			   (union-tree (right-branch a)
				       (right-branch b))))
	       ((< (entry a) (entry b))
		;; now ?
		)))))


(TEST
 > (tree->list t1)
 (3 4 5 7 8 10 11)
 > (tree->list t2)
 (1 4 5 9 38)
 > (tree->list (union-tree t1 t2))
 (1 3 4 5 7 8 9 10 11 38)
 )

