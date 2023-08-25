#lang eopl
(require "../eopl-extras.rkt")

(provide zero isZero? succ pred dec2nnint nnint2dec)

#|

A nnint is a nonnegative Racket integer

|#

; Constructors

;;  --> nnint
;; Purpose: Construct zero
(define (zero) 0)

;; number --> nnint
;; Purpose: Construct nnint for the given decimal nnint
(define (dec2nnint n) n)

;; nnint --> nnint
;; Purpose: Construct the successor of the given nnint
(define succ add1)

; Observers
;; nnint --> Boolean
;; Purpose: Determine if given nnint is zero
(define isZero? zero?)

;; nnint --> nnint
;; Purpose: Return the predecessor of given nnint
(define (pred n)
  (if (= n 0)
      (eopl:error 'pred "Zero does not have a predecessor.")
      (sub1 n)))

;; nnint --> number
;; Purpose: Return decimal representation of given nnint
(define (nnint2dec n) n)


