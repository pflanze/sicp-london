(define (id cont v)
  (cont v))

(define (main x)
  (display x) (newline))

(define (t)
  (id main 1))


(define (factorial cc n)
  (if (= n 0)
      (cc 1)
      (factorial (lambda (x)
		   (cc (* n x)))
		 (- n 1))))


(define sc #f)
(define (save! c)
  (call/cc (lambda (cc)
	     (set! sc cc)
	     x)))

;; ------------------------------------------------------------------

(define (current-continuation)
  (call/cc (lambda (cc) cc)))

(define (tt)
  (let ((cc (current-continuation)))
    ;; return 42:
    ;;42
    ;; endless loop:
    ;;(cc cc)
    ;; "0 not a procedure":
    (cc 0)))

;; <not-part-of-meetup>

;; partially CPS transformed variant:

(define (partialcps-tt)
  ((lambda (cc)
     ;; return 42:
     ;;42
     ;; endless loop:
     ;;(cc cc)
     ;; "0 not a procedure":
     (cc 0))
   (current-continuation)))


;; fully CPS transformed variant (works even if the underlying Scheme
;; system doesn't have call/cc). A continuation is a function of one
;; two arguments, a continuation and a value (not supporting multiple
;; value 'returns' here). Using "cps:" prefix just to distinguish the
;; names.

(define (cps:main val)
  val)

(define (cps:call/cc continuation fn)
  (fn continuation continuation))

(define (cps:current-continuation continuation)
  (cps:call/cc continuation
	       (lambda (cc)
		 (continuation cc))))

;; Call with: (cps:tt cps:main)
(define (cps:tt continuation)
  (cps:current-continuation
   (lambda (cc)
     ;; return 42:
     (continuation 42)
     ;; endless loop: 
     ;;(cc continuation cc)
     ;; "0 not a procedure":
     ;;(cc continuation 0)
     )))

;; </not-part-of-meetup>

;; ------------------------------------------------------------------
;; label, goto

(define label current-continuation)
;; or (define (label) (current-continuation))
;;  which does the same (but transparently wraps another parameterless function) 

(define (goto l)
  (l l))

(define (t3)
  (define c 0)
  (let ((l (label)))
    (println c)
    (set! c (+ c 1))
    (if (< c 10)
	(goto l))))


;; ..which is the same as (let turned into lambda, and inlined goto
;; call):

(define (t3b)
  (define c 0)
  ((lambda (l)
     (println c)
     (set! c (+ c 1))
     (if (< c 10)
	 (l l)))
   (current-continuation)))


;; ------------------------------------------------------------------
;; AMB

;; 'such that (amb ) will pick an element of the list that satisfies
;; the asserts that come later'

(define *stack* '())

(define (next-from-stack)
  (if (null? *stack*)
      (error "out of things to try")
      (let ((cont (car *stack*)))
	(set! *stack* (cdr *stack*))
	(cont cont))))

(define (amb l)
  (call/cc
   (lambda (return)
      (define (lp l)
	(if (null? l)
	    (next-from-stack)
	    (let ((x (car l))
		  (l* (cdr l)))
	      (call/cc (lambda (cc)
			  (set! *stack* (cons cc *stack*))
			  (return x)))
	      (lp l*))))
      (lp l))))

(define (assrt c)
  (if (not c)
      (next-from-stack)))

(define (t5)
  ;;(step)
  (let ((a (amb (list 1 2 3 4 5 6 7)))
	(b (amb (list 1 2 3 4 5 6 7)))
	(c (amb (list 1 2 3 4 5 6 7))))
    ;; (warn "testing:"
    ;; 	  (list a b c)
    ;; 	  *stack*)
    (assrt (= (* c c) (+ (* a a) (* b b))))
    (assrt (< b a))
    (list a b c)
    ;; to show all matching results:
    ;;(warn "FOUND:" (list a b c))
    ;;(next-from-stack)
    ))

