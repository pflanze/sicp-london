
;;(define displayln println)
(define (displayln . args)
  (for-each write args)
  (newline))
(define false #f)
(define (NOTE . args)
  (void))

;; Huffman use variable-length prefix codes that take advantage 
;; of the relative frequencies of the symbols in the messages to be encoded


;; helper nethods from secr 2.3.3
(define (element-of-set? x items)
  (cond ((null? items) false) 
	((equal? x (car items)) true) 
	(else (element-of-set? x (cdr items)))))

(define set-union append)


(define-struct. weightset
  sorted-items)

(define empty-weightset (weightset '()))

;; (define (element-of-weightset? x set)
;;   (let next ((items (weightset.sorted-items set)))
;;     (cond ((null? items) false) 
;; 	  ((equal? x (car items)) true) 
;; 	  (else (next x (cdr items))))))

;; construct a set by comparing weights note x is never in the set
(define (adjoin-weightset x set)
  (weightset
   (let next ((x x)
	      (items (weightset.sorted-items set)))
     (cond ((null? items) (list x))
	   ((< (weight x) (weight (car items))) (cons x items))
	   (else (cons (car items)
		       (next x (cdr items))))))))

(TEST
 > (define flipped-adjoin-weightset (flip adjoin-weightset))
 > (define t
     (chain empty-weightset
	    (flipped-adjoin-weightset (leaf 'A 3))
	    (flipped-adjoin-weightset (leaf 'B 3))
	    (flipped-adjoin-weightset (leaf 'C 4))
	    (flipped-adjoin-weightset (leaf 'E 10))))
 > t
 #(weightset (#(leaf A 3) #(leaf B 3) #(leaf C 4) #(leaf E 10)))
 > (adjoin-weightset (leaf 'F 11) t)
 #(weightset (#(leaf A 3) #(leaf B 3) #(leaf C 4) #(leaf E 10) #(leaf F 11)))
 > (adjoin-weightset (leaf 'F 1) t)
 #(weightset (#(leaf F 1) #(leaf A 3) #(leaf B 3) #(leaf C 4) #(leaf E 10)))
 )


;; the leafs of a Huffman tree ex: (A 8)
(define-struct. leaf
  #(symbol? symbol)
  #(natural? weight))

;; leaf tests
(define l (leaf 'A 8))
(define r (leaf 'H 1))
(NOTE " ++ leaf")
(NOTE l)
(NOTE r)

;; A general tree will have a left branch, a right branch,
;; a set of symbols, and a weight.
(define-struct. tree
  #(tree-or-leaf? left)
  #(tree-or-leaf? right)
  symbols
  #(natural? weight))

(define tree-or-leaf? (either tree? leaf?))

;; The set of symbols will simply 
;; be a simple list of the symbols
(define (code-tree left right)
  (tree left
	right
	(set-union (symbols left) (symbols right))
	(+ (weight left) (weight right))))

;; generic selectors

(define (symbols tree)
  (if (leaf? tree)
      (list (leaf.symbol tree))
      (tree.symbols tree)))

(define (weight tree)
  (if (leaf? tree)
      (leaf.weight tree)
      (tree.weight tree)))


;; tree tests
(define my-huff (code-tree (leaf 'A 8) (leaf 'B 2)))
(NOTE " ++ code-tree")
(NOTE my-huff)


;; decode a huffman tree when supplied with a list of 1s and zeros and a Hufftree
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (leaf.symbol next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (tree.left branch))
        ((= bit 1) (tree.right branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


;; takes a set of pairs ex  ((A 4) (B 2) (C 1) (D 1)) and constructs an initial set of leaves

;; "constructs an initial ordered set of leaves, ready to be merged
;; according to the Huffman algorithm"
;; Those with the lowest weights come *first*

(define (leaf-set pairs)
  (if (null? pairs)
      empty-weightset
      (let ((pair (car pairs)))
        (adjoin-weightset (leaf (car pair)   ; symbol
				(cadr pair)) ; frequency
			  (leaf-set (cdr pairs))))))

(TEST
 > (leaf-set '())
 #(weightset ())
 > (leaf-set '((A 4)))
 #(weightset (#(leaf A 4)))
 > (leaf-set '((A 4) (B 2)))
 #(weightset (#(leaf B 2) #(leaf A 4)))
 > (leaf-set '((A 4) (D 1) (B 2) (C 1)))
 #(weightset (#(leaf C 1) #(leaf D 1) #(leaf B 2) #(leaf A 4)))
 )


;; Exercise 2.67.  Define an encoding tree and a sample message:
(NOTE " ++ decoding")

(define sample-tree
  (code-tree (leaf 'A 4)
	     (code-tree
	      (leaf 'B 2)
	      (code-tree (leaf 'D 1)
			 (leaf 'C 1)))))
(NOTE " ++ tree")
(NOTE sample-tree)

(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(NOTE " ++ bits")
(NOTE sample-bits)

(define sample-message (decode sample-bits sample-tree))
(NOTE " ++ result")
(NOTE sample-message)

;; Ex 2.68 Encode a message with supplied Huffman tree
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond ((leaf? tree) '())
	((element-of-set? sym (symbols (tree.left tree)))
	 (cons 0 (encode-symbol sym (tree.left tree))))
	((element-of-set? sym (symbols (tree.right tree)))
	 (cons 1 (encode-symbol sym (tree.right tree))))
	(else
	 (error "??"))))


;; ex 2.69. 
;; The following procedure takes as its argument a list of symbol-frequency pairs
;; (where no symbol appears in more than one pair) and generates a Huffman encoding
;; tree according to the Huffman algorithm. Write successive-merge
(define (generate-huffman-tree pairs)
  (successive-merge (leaf-set pairs)))

;; take '((A 3) (B 5) (C 6) (D 6)), merge those subtrees with (equal?) lowest weights

(define (successive-merge leafset)
  (let* ((items (weightset.sorted-items leafset))
	 (t (code-tree (car items)
		       (cadr items)))
	 (rest (cddr items)))
    ;;    (step)
    (if (null? rest)
	t
	(successive-merge (adjoin-weightset t (weightset rest))))))


;; --------------------------------------------------------------------
;; tests
(NOTE " ++ successive merge")
(define test-tree (generate-huffman-tree '((B 5) (A 3) (D 6) (C 6)))) 


(define (random-mesg len numsyms)
  (define syms (list->vector (map (lambda (x)
				    (string.symbol (string (.char (+ (.integer #\A) x)))))
				  (iota numsyms))))
  (map (lambda (_)
	 (vector-ref syms (random-integer numsyms)))
       (iota len)))

(TEST
 > (equal? sample-bits (encode sample-message sample-tree))
 #t
 > (encode '(A B C D) test-tree)
 (0 0 0 1 1 0 1 1)
 > (decode # test-tree)
 (A B C D)

 > (define (test len numsyms)
     (let* ((m (random-mesg len numsyms))
	    (e (encode m test-tree)))
       ;;(step)
       (equal? m (decode e test-tree))))
 > (test 20 4)
 #t
 > (test 200 3)
 #t
 )
