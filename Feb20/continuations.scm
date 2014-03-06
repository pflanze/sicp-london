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

(define (identity x) x)

(define (current-continuation)
  (call/cc identity))

(define (tt)
  (let ((cc (current-continuation)))
    ;; returning 42:
    ;;42
    ;; going into an endless loop:
    ;;(cc cc)
    ;; hitting "0 is not a procedure":
    (cc 0)))

;; <not-part-of-meetup>

;; partially CPS transformed variant:

(define (partialcps:tt)
  ((lambda (cc)
     ;; returning 42:
     ;;42
     ;; going into an endless loop:
     ;;(cc cc)
     ;; hitting "0 is not a procedure":
     (cc 0))
   (current-continuation)))


;; Fully CPS transformed variant (which means it works without relying
;; on call/cc of the Scheme interpreter (in fact it works without
;; using implicit continuations ("returns") at all, except for
;; cps:main)). A continuation is a function of one or more arguments,
;; values (usually one, multiple of them are supported in Scheme by
;; way of the |values| and |call-with-values| procedures, and in our
;; CPS transformed code by way of the cps:-prefixed equivalents
;; you find defined further down).
;; Using a "cps:" prefix just to distinguish the names.

(define cps:main identity) ;; the initial continuation, which 'lifts'
			   ;; (returns) the end result to the normal
			   ;; Scheme continuation (which is the
			   ;; 'print' in "repl").

(define (cps:identity cont x)
  (cont x))

(define (cps:call/cc cont fn)
  (fn cont
      ;; we need to wrap cont so that we follow the CPS function
      ;; calling convention:
      (lambda (ignored-cont . vals)
	(apply cont vals))))

(define (cps:current-continuation cont)
  (cps:call/cc cont cps:identity))

;; Call with: (cps:tt cps:main)
(define (cps:tt cont)
  (cps:current-continuation
   ;; (this lambda is representing a continuation, not a function: )
   (lambda (cc)
     ;; (^ but cc is a function that wraps a continuation)

     ;; returning 42:
     ;;(cont 42)
     ;; going into an endless loop:
     ;;(cc cont cc)
     ;; hitting "0 is not a procedure":
     (cc cont 0)
     )))


;; Multiple value support:

;; Normal Scheme:
;; > (call-with-values (lambda ()
;;                        (values 1 2 3))
;;                     (lambda (a b c)
;;                        b))
;; 2

(define (cps:call-with-values cont producer consumer)
  ;; If Scheme had automatic currying, it would be:
  ;;    (producer (consumer cont))
  (producer (lambda vals
	      (apply consumer cont vals))))

(define (cps:values cont . vals)
  (apply cont vals))

;; > (cps:call-with-values cps:main
;; 		      (lambda (cont)
;; 			(cps:values cont 1 2 3))
;; 		      (lambda (cont a b c)
;; 			(cont b)))
;; 2

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

