(define (good-enough? guess x)
(< (abs (- (* guess guess guess) x)) 0.001))

(define (newapprox x y)
(/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (cube-iter guess x)
(if (good-enough? guess x)
guess
(cube-iter (newapprox x guess) x)))

(write (cube-iter 1 8.0))
