#lang eopl
(require "../eopl-extras.rkt")

(provide zero isZero? succ pred dec2nnint nnint2dec)

#|

A nnint is either
  1. '()
  2. (cons #t nnint)

|#

; Constructors

;;  --> nnint
;; Purpose: Construct zero
(define (zero) '())

;; number --> nnint
;; Purpose: Construct nnint for the given decimal nnint
(define (dec2nnint n)
  (build-list n (lambda (i) #t)))

;; nnint --> nnint
;; Purpose: Construct the successor of the given nnint
(define (succ n) (cons #t n))

; Observers
;; nnint --> Boolean
;; Purpose: Determine if given nnint is zero
(define (isZero? n) (null? n))

;; nnint --> nnint
;; Purpose: Return the predecessor of given nnint
(define pred cdr)

;; nnint --> number
;; Purpose: Return decimal representation of given nnint
(define (nnint2dec n) (length n))


