#lang eopl
(require rackunit)

(define-datatype bintree bintree?
  (leaf-node (data number?))
  (interior-node
   (key symbol?)
   (lst bintree?)
   (rst bintree?)))

;; Sample bintree
(define BT0 (leaf-node 4))
(define BT1 (interior-node 'T (leaf-node 2) (leaf-node -2)))
(define BT2 (interior-node 'T
                           (interior-node
                             'L
                             (leaf-node 10)
                             (leaf-node 20))
                           (interior-node
                             'L
                             (leaf-node 30)
                             (leaf-node 40))))
                                          

;; bintree --> number
;; Purpose: Add nums in given bintree
(define (leaf-sum t)
  (cases bintree t
    (leaf-node (val) val)
    (interior-node (k l r)
                   (+ (leaf-sum l) (leaf-sum r)))))

(check-equal? (leaf-sum BT0) 4)
(check-equal? (leaf-sum BT1) 0)
(check-equal? (leaf-sum BT2) 100)
