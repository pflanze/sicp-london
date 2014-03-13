(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

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



(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))



;; The following procedure takes a list of symbol-frequency pairs such
;; as ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set
;; of leaves, ready to be merged according to the Huffman algorithm:


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

;; (define sample-tree
;;   (make-code-tree (make-leaf 'A 4)
;;                   (make-code-tree
;;                    (make-leaf 'B 2)
;;                    (make-code-tree (make-leaf 'D 1)
;;                                    (make-leaf 'C 1)))))

;; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))




;; turn (sorted?) frequencies into 

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; write successive-merge

(define expairs
  (sort '((A 1)
	  (B 3)
	  (C 8)
	  (D 4)
	  (E 8)
	  (F 1))
	(on cadr >)))


;; Huffman tree:

;; has   binary  left right  on  sets  if part of set? . but what is weight for?
;;???

;; merge 2 of same

;; At each step, merge two nodes with the smallest weights, removing
;; them from the set and replacing them with a node that has these two
;; as its left and right branches.

;; The process stops when there is only one node left, which is the
;; root of the entire tree.
;; ((what about 2?--well merge?))

(define (successive-merge s)
  )