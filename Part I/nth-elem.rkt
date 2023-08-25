#lang eopl
(require rackunit)

;; (listof X) ::= '() | (cons X (listof X))

;; (listof X) natnum --> X
;; Purpose: Extract the nth element of the given list
(define (nthelem l n)
  (cond [(null? l)
         (eopl:error 'nthelem "List too short by ~s elems" (+ n 1))]
        [(zero? n) (car l)]
        [else (nthelem (cdr l) (- n 1))]))

(check-equal? (nthelem '(0 1 2 3) 3) 3)
(check-equal? (nthelem '(a b c d e f g h) 5) 'f)