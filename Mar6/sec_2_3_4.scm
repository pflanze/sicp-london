
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

;; takes a set of pairs ex  ((A 4) (B 2) (C 1) (D 1)) and constructs an initial set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


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

;; proc that returns the list of bits that encodes a given symbol according to a given tree.
;; solution ripped from web
(define (encode-symbol symb tree) 
  (define (branch-correct? branch) 
    (if (leaf? branch) 
        (equal? symb (symbol-leaf branch)) 
        (element-of-set? symb (symbols branch)))) 
  
  (let ((lb (left-branch tree)) 
        (rb (right-branch tree))) 
    (cond ((branch-correct? lb) 
           (if (leaf? lb) '(0) (cons 0 (encode-symbol symb lb)))) 
          ((branch-correct? rb) 
           (if (leaf? rb) '(1) (cons 1 (encode-symbol symb rb)))) 
          (else (error "bad symbol -- ENCODE-SYMBOL" symb)))))                                          

;; tests
(encode '(A D A B B C A) sample-tree) 
(equal? sample-bits (encode sample-message sample-tree))

;; ex 2.69. 
;; The following procedure takes as its argument a list of symbol-frequency pairs
;; (where no symbol appears in more than one pair) and generates a Huffman encoding
;; tree according to the Huffman algorithm. Write successive-merge
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; ripped from web
;; Ordered sets may be implemented as lists, but abstraction dictates that we should 
;; never use list operators on them directly! 
;; By using a few name changes and methods we can respect the abstraction and remind 
;; ourselves what the objects actually are.
(define (successive-merge tree-ordered-set) 
  (if (= (size-of-set tree-ordered-set) 1) 
      (first-in-ordered-set tree-ordered-set) 
      (let ((first (first-in-ordered-set tree-ordered-set)) 
            (second (second-in-ordered-set tree-ordered-set)) 
            (rest (subset tree-ordered-set 2))) 
        (successive-merge (adjoin-set (make-code-tree first second) 
                                      rest))))) 

(define size-of-set length) 
(define first-in-ordered-set car) 
(define second-in-ordered-set cadr) 
(define (subset set n) 
  (if (= n 0) 
      set  
      (subset (cdr set) (- n 1))))

;; tests
(NOTE " ++ successive merge")
(define test-tree (generate-huffman-tree '((A 3) (B 5) (C 6) (D 6)))) 

(TEST
 > (encode '(A B C D) test-tree)
 (0 0 0 1 1 1 1 0)
 > (decode # test-tree)
 (A B C D)
 )
