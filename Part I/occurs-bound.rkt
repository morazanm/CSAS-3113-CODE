#lang eopl

(require rackunit "occurs-free.rkt")

;; symbol exp --> Boolean
;; Purpose: Determine if given variable occurs bound in given exp
(define (occurs-bound? x exp)				
  (cond	[(or (number? exp) (boolean? exp) (symbol? exp)) #f]
        [(eqv? (car exp) 'lambda)
         (or (occurs-bound? x (caddr exp))
             (and (member x (cadr exp))
                  (occurs-free? x (caddr exp))))]
        [else (or (occurs-bound? x (car exp))
                  (occurs-bound? x (cadr exp)))]))

(check-equal? (occurs-bound? 'x 187) #f)
(check-equal? (occurs-bound? 'a #t) #f)
(check-equal? (occurs-bound? 'a 'b) #f)
(check-equal? (occurs-bound? 'a '(lambda (a b) (f (g b b)))) #f)
(check-equal? (occurs-bound? 'a '(lambda (x b) (f (g a b x)))) #f)
(check-equal? (occurs-bound? 'b '((lambda (a x) (times 2 x a)) b)) #f)
(check-equal? (occurs-bound? 'a '(lambda (a b) a)) #t)
(check-equal? (occurs-bound? 'a '(lambda (a b) (f (g a b)))) #t)
(check-equal? (occurs-bound? 'z '((lambda (z x) (times 2 x z)) b)) #t)