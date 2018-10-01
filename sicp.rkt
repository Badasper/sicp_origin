#lang racket/base
; settings
(define MAX-BYTES (* 1024 1000000))
(custodian-limit-memory (current-custodian) MAX-BYTES)
; end settings

; SICP original Yakovlev Alexander

; exercise 1.6
(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2)) 

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.01))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; new-if -> recursion because normal evaluation form (first eval all parameters of func)
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(sqrt-iter 1.3 10)

; exercise 1.7
(define (new-good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess))
          (* new-guess 0.01)))

(define (new-sqrt guess x)
  (define (sqrt-iter-guess old-guess new-guess x)
    (if (new-good-enough? old-guess new-guess)
      new-guess
      (sqrt-iter-guess new-guess
                       (improve new-guess x)
                       x)))
  (sqrt-iter-guess x guess x))

(new-sqrt 1.3 10)

; exercise 1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3.0))


(define (sqrt-cube guess x)
  (if (new-good-enough? guess (improve-cube guess x))
    (improve-cube guess x)
    (sqrt-cube (improve-cube guess x)
               x)))

(sqrt-cube 4 27)

; exercise 1.9
(define (inc a)
  (+ 1 a))

  (define (dec a)
    (- a 1))

(define (++ a b)
  (if (= a 0)
    b
    (inc (++ (dec a) b))))
(++ 4 5)

(define (+++ a b)
  (if (= a 0)
    b
    (+++ (dec a) (inc b))))
(+++ 4 5)

; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)


; exercise 1.11
; f(n) = n if n < 3; f(n) = f(n-1) + f(n-2) + f(n-3) if n>=3

(define (custom-fn-recursion n)
  (cond ((< n 3) n)
    (else (+ (custom-fn-recursion (- n 1))
             (custom-fn-recursion (- n 2))
             (custom-fn-recursion (- n 3))))))

(custom-fn-recursion 2)

(define (count-change amount) 
  (cc amount 5)) 
(define (cc amount kinds-of-coins) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (= kinds-of-coins 0)) 0) 
        (else (+ (cc amount 
                     (- kinds-of-coins 1)) 
                 (cc (- amount 
                        (first-denomination kinds-of-coins)) 
                     kinds-of-coins))))) 
(define (first-denomination kinds-of-coins) 
  (cond ((= kinds-of-coins 1) 50) 
        ((= kinds-of-coins 2) 25) 
        ((= kinds-of-coins 3) 10) 
        ((= kinds-of-coins 4) 5) 
        ((= kinds-of-coins 5) 1))) 

(count-change 11)

; exercise 1.16

(define (even? n)
  (= (remainder  n 2) 0))

(define (fast-expt-iter b n)
  (define (iter acc count)
    (cond ((= count n) acc)
      ((even? n) (iter (* acc (square b)) (+ count 2)))
      (else (iter (* acc b) (+ count 1)))))
  (iter 1 0))


(fast-expt-iter 2 8)

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 36 6)

