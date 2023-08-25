#lang eopl

(require rackunit "../eopl-extras.rkt")

(define (ints-from n) (stream-cons n (ints-from (+ n 1))))

(define natnums (ints-from 0))

(define (nth-natnum n) (stream-ref natnums n))

(define (first-n-natnums n)
  (if (= n 0)
      (list (nth-natnum 0))
      (cons (nth-natnum n) (first-n-natnums (- n 1)))))

(check-equal?  (first-n-natnums 10)
               '(10 9 8 7 6 5 4 3 2 1 0))

(check-equal?  (first-n-natnums 15)
               '(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))



;; natnum --> natnum
;; Purpose: Return the kth Fibonacci number
(define (fib k)
  (if (< k 2)
      1
      (+ (fib (- k 1)) (fib (- k 2)))))

(define (the-fibs n) (stream-cons (fib n) (the-fibs (+ n 1))))

(define fibs (the-fibs 0))

(define (nth-fib n) (stream-ref fibs n))

(check-equal?  (nth-fib 5)  8)

(check-equal?  (nth-fib 10) 89)



(define the-doubles (stream-map (λ (n) (* 2 n)) natnums))

(check-equal? (stream-ref the-doubles 10) 20)

(check-equal? (stream-ref the-doubles 1287) 2574)