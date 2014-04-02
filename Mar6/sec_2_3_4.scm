
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
(define (element-of-set? x set)
  (cond ((null? set) false) 
        ((equal? x (car set)) true) 
        (else (element-of-set? x (cdr set))))) 



;; the leafs of a Huffman tree ex: (A 8)
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; leaf tests
(define l (make-leaf 'A 8))
(define r (make-leaf 'H 1))
(NOTE " ++ leaf")
(NOTE l)
(NOTE r)

;; A general tree will be a list of a left branch, a right branch,
;; a set of symbols, and a weight. The set of symbols will simply 
;; be a simple list of the symbols
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;; tree tests
(define my-huff (make-code-tree (make-leaf 'A 8) (make-leaf 'B 2)))
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
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; construct a set by comparing weights note x is never in the set
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(TEST
 > (adjoin-set '(leaf A 3) '())
 ((leaf A 3))
 > (adjoin-set '(leaf B 3) #)
 ((leaf A 3) (leaf B 3))
 > (adjoin-set '(leaf C 4) #)
 ((leaf A 3) (leaf B 3) (leaf C 4))
 > (adjoin-set '(leaf E 10) #)
 ((leaf A 3) (leaf B 3) (leaf C 4) (leaf E 10))
 > (define t #)
 > (adjoin-set '(leaf F 11) t)
 ((leaf A 3) (leaf B 3) (leaf C 4) (leaf E 10) (leaf F 11))
 > (adjoin-set '(leaf F 1) t)
 ((leaf F 1) (leaf A 3) (leaf B 3) (leaf C 4) (leaf E 10))
 )
;; takes a set of pairs ex  ((A 4) (B 2) (C 1) (D 1)) and constructs an initial set of leaves

;; "constructs an initial ordered set of leaves, ready to be merged
;; according to the Huffman algorithm"
;; Those with the lowest weights come *first*

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(TEST
 > (make-leaf-set '())
 ()
 > (make-leaf-set '((A 4)))
 ((leaf A 4))
 > (make-leaf-set '((A 4) (B 2)))
 ((leaf B 2) (leaf A 4))
 > (make-leaf-set '((A 4) (D 1) (B 2) (C 1)))
 ((leaf C 1) (leaf D 1) (leaf B 2) (leaf A 4))
 )


;; Exercise 2.67.  Define an encoding tree and a sample message:
(NOTE " ++ decoding")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
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
	((element-of-set? sym (symbols (left-branch tree)))
	 (cons 0 (encode-symbol sym (left-branch tree))))
	((element-of-set? sym (symbols (right-branch tree)))
	 (cons 1 (encode-symbol sym (right-branch tree))))
	(else
	 (error "??"))))


;; ex 2.69. 
;; The following procedure takes as its argument a list of symbol-frequency pairs
;; (where no symbol appears in more than one pair) and generates a Huffman encoding
;; tree according to the Huffman algorithm. Write successive-merge
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; take '((A 3) (B 5) (C 6) (D 6)), merge those subtrees with (equal?) lowest weights

(define (successive-merge leafset)
  (let ((t (make-code-tree (car leafset)
			   (cadr leafset)))
	(rest (cddr leafset)))
;;    (step)
    (if (null? rest)
	t
	(successive-merge (cons t rest)))))


;; --------------------------------------------------------------------
;; tests
(NOTE " ++ successive merge")
(define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6)))) 


(define (random-mesg len numsyms)
  (define syms (list->vector (map (lambda (x)
				    (string.symbol (string (.char (+ (.integer #\A) x)))))
				  (iota numsyms))))
  (map (lambda (_)
	 (vector-ref syms (random-integer numsyms)))
       (iota len)))

(TEST
 > (encode '(A D A B B C A) sample-tree) 
 > (equal? sample-bits (encode sample-message sample-tree))
 #t
 > (encode '(A B C D) test-tree)
 (0 0 0 1 1 1 1 0)
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
