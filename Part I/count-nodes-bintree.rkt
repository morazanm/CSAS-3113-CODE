#lang eopl
(require rackunit)

#|

bintree --> number
        --> (number bintree bintree)

|#

;; bintree --> odd-natnum
;; Purpose: Count the nodes the given bintree
(define (cnt-nodes s)
  (if (number? s)
      1
      (+ 1 (cnt-nodes (cadr s)) (cnt-nodes (caddr s)))))

(check-equal? (cnt-nodes 23) 1)
(check-equal? (cnt-nodes (list 45 88 6561)) 3)
(check-equal? (cnt-nodes (list 45 (list 88 11 99) (list 6561 -6 42))) 7)