#lang eopl

(require rackunit "unary-nnints.rkt")
;(require rackunit "racketnums-nnint.rkt")


;; nnint nnint --> nnint
;; Purpose: Add the given nnints
(define (plus x y)
  (if (isZero? x)
      y
      (succ (plus (pred x) y))))

(check-equal? (nnint2dec (plus (dec2nnint 0) (dec2nnint 0))) 0)
(check-equal? (nnint2dec (plus (dec2nnint 2) (dec2nnint 0))) 2)
(check-equal? (nnint2dec (plus (dec2nnint 0) (dec2nnint 1))) 1)
(check-equal? (nnint2dec (plus (dec2nnint 3) (dec2nnint 2))) 5)