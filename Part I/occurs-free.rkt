#lang eopl

(require rackunit "../eopl-extras.rkt")

(provide occurs-free?)

#|

exp --> id
    --> number
    --> Boolean
    --> (lambda (id*) exp)
    --> (exp exp*)

|#
;; symbol exp --> Boolean
;; Purpose: Determine if the given variable occurs free in the given expression
(define (occurs-free? x exp)
  (cond	[(or (number? exp) (boolean? exp)) #f]
        [(symbol? exp) (eqv? x exp)]
        [(eqv? (car exp) 'lambda)
         (and (not (member x (cadr exp)))
              (occurs-free? x (caddr exp)))]
        [else (or (occurs-free? x (car exp))
                  (ormap (lambda (e) (occurs-free? x e)) (cdr exp)))]))

(check-equal? (occurs-free? 'x 187) #f)
(check-equal? (occurs-free? 'a #t) #f)
(check-equal? (occurs-free? 'a 'b) #f)
(check-equal? (occurs-free? 'a '(lambda (a b) (f (g a b)))) #f)
(check-equal? (occurs-free? 'a '(lambda (a b) (f (g b b)))) #f)
(check-equal? (occurs-free? 'b '((lambda (a x) (times 2 x a)) b)) #t)
(check-equal? (occurs-free? 'a '(lambda (x b) (f (g a b x)))) #t)
(check-equal? (occurs-free? 'a '((lambda (z x) (times 2 x a)) b)) #t)


