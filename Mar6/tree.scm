

(define empty-set '())
(define empty-set? null?)

;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.3
;; Sets as binary trees

(define-struct tree
  entry
  left
  right)

(define entry tree-entry)
(define left-branch tree-left)
(define right-branch tree-right)

(define (element-of-set? x set)
  (cond ((empty-set? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; union-set
;; intersection-set

;; bonus: O(n)

(define t1
  (make-tree 5
	     (make-tree 4
			(make-tree 3 empty-set empty-set)
			empty-set)
	     (make-tree 7
			empty-set
			(make-tree 10
				   (make-tree 8 empty-set empty-set)
				   (make-tree 11 empty-set empty-set)))))


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
 ;; > (list->tree '(1 2 3 4 5 6 7))
 ;; (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))
 ;; hm
 ;; #(tree
 ;;   4
 ;;   #(tree 2 #(tree 1 () ()) #(tree 3 () ()))
 ;;   #(tree 6 #(tree 5 () ()) #(tree 7 () ())))
 
 > (define t2 (list->tree '(1 4 5 9 38)))
 )


(define (union-sorted a b)
  (cond ((empty-set? a)
	 b)
	((empty-set? b)
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
  (cond ((empty-set? a) b)
	((empty-set? b) a)
	(else
	 (let ((make
		   (lambda (t1 t2) ;; t1 has smaller entry than t2
		     (adjoin-set (entry t1)
				 (adjoin-set (entry t2)
					     (union-tree
					      (union-tree (right-branch t1)
							  (left-branch t2))
					      (union-tree (left-branch t1)
							  (right-branch t2))))))))
	   (cond ((= (entry a) (entry b))
		  (make-tree (entry a)
			     (union-tree (left-branch a)
					 (left-branch b))
			     (union-tree (right-branch a)
					 (right-branch b))))
		 ;; Strategy?
		 ;; - pick 
		 ((< (entry a) (entry b))
		  ;; now ?
		  ;; We need to make at least one of a or b smaller.
		  (make a b))
		 (else
		  (make b a)))))))


(TEST
 > (tree->list t1)
 (3 4 5 7 8 10 11)
 > (tree->list t2)
 (1 4 5 9 38)
 > (tree->list (union-tree t1 t2))
 (1 3 4 5 7 8 9 10 11 38)
 )


;; ---- more tests: ---------------------------------------------

(define (randomtree add start n)
  (let ((is (map (lambda (_) (random-integer n)) (iota n))))
    (values is
	    (fold add start is))))

;; an |and| that errors for non-true values
(define-macro* (xand . es)
  (let rec ((es es))
    (if (null? es)
	`#t
	`(if ,(car es)
	     ,(rec (cdr es))
	     ;; XX only carry over location!  location-error, pls
	     (source-error (source-dequote ',(source-quote (car es)))
			   "xand: got false")))))

(define (rtest n)
  (letv ((is t) (randomtree adjoin-set empty-set n))
	(let ((s0 (list-uniq = (sort is <))))
	  (xand (equal? s0 (tree->list t))
		(let ((all (iota n)))
		  (letv ((is2 nonis) (partition (C element-of-set? _ t) all))
			(xand (equal? is2 s0)
			      (equal? (sort (append s0 nonis) <) all))))))))

(TEST
 > (rtest 100)
 #t
 > (rtest 1000)
 #t
 > (rtest 10000)
 #t
 )

