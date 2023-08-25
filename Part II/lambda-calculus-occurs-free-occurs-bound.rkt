#lang eopl

(require rackunit "../eopl-extras.rkt")

#|

exp --> id
    --> number
    --> Boolean
    --> (lambda (id*) exp)
    --> (exp exp*)

|#

;; Sample exp (concrete syntax)
(define E0 'x)
(define E1 100)
(define E2 #t)
(define E3 '(lambda (x y) (+ x y)))
(define E4 '((lambda (x y) (+ x z)) 2 3))

(define-datatype expr expr?
  (var-expr (id symbol?))
  (num-expr (num number?))
  (bool-expr (b boolean?))
  (lambda-expr
   (params (list-of symbol?))
   (body expr?))
  (app-expr
   (op expr?)
   (args (list-of expr?))))

;; exp --> expr
;; Purpose: Parse the give LC exp
(define (parse-lc-exp e)
  (cond [(symbol? e) (var-expr e)]
        [(number? e) (num-expr e)]
        [(boolean? e) (bool-expr e)]
        [(eq? (car e) 'lambda)
         (lambda-expr (cadr e)
                      (parse-lc-exp (caddr e)))]
        [else (app-expr
               (parse-lc-exp (car e))
               (map (lambda (aexp) (parse-lc-exp aexp))
                    (cdr e)))]))

;; expr --> exp
;; Purpose: Unparse the given LC expr
(define (unparse-lc-expr er)
  (cases expr er
    (var-expr (s) s)
    (num-expr (n) n)
    (bool-expr (b) b)
    (lambda-expr (params body)
                 (list 'lambda params (unparse-lc-expr body)))
    (app-expr (op args)
              (cons (unparse-lc-expr op)
                    (map (lambda (exr) (unparse-lc-expr exr))
                         args)))))

(check-equal? (unparse-lc-expr (parse-lc-exp E0)) E0)
(check-equal? (unparse-lc-expr (parse-lc-exp E1)) E1)
(check-equal? (unparse-lc-expr (parse-lc-exp E2)) E2)
(check-equal? (unparse-lc-expr (parse-lc-exp E3)) E3)
(check-equal? (unparse-lc-expr (parse-lc-exp E4)) E4)

;; symbol expr --> Boolean
;; Purpose: Determine if given var occurs free in given expr
(define (occurs-free? x exp)
  (cases expr exp
    (var-expr (id)  (eqv? x id))
    (num-expr (n) #f)
    (bool-expr (b) #f)
    (lambda-expr (ids body)
                 (and (not (member x ids))
                      (occurs-free? x body)))
    (app-expr (op args)
              (ormap (lambda (e) (occurs-free? x e))
                     (cons op args)))))

(check-equal? (occurs-free? 'a (parse-lc-exp E0)) #f)
(check-equal? (occurs-free? 'a (parse-lc-exp E1)) #f)
(check-equal? (occurs-free? 'a (parse-lc-exp E2)) #f)
(check-equal? (occurs-free? 'y (parse-lc-exp E3)) #f)
(check-equal? (occurs-free? 'y (parse-lc-exp E4)) #f)
(check-equal? (occurs-free? 'x (parse-lc-exp E0)) #t)
(check-equal? (occurs-free? 'z (parse-lc-exp E4)) #t)


;; symbol expr --> Boolean
;; Purpose: Determine if given var occurs bound in given expr
(define (occurs-bound? x exp)
  (cases expr exp
    (var-expr (id) #f)
    (num-expr (n)  #f)
    (bool-expr (b) #f)
    (lambda-expr (ids body)
                 (or (occurs-bound? x body)
                     (and (member x ids)
                          (occurs-free? x body))))
    (app-expr (op args)
              (ormap (lambda (e) (occurs-bound? x e))
                     (cons op args)))))

(check-equal? (occurs-bound? 'a (parse-lc-exp E0)) #f)
(check-equal? (occurs-bound? 'x (parse-lc-exp E0)) #f)
(check-equal? (occurs-bound? 'a (parse-lc-exp E1)) #f)
(check-equal? (occurs-bound? 'a (parse-lc-exp E2)) #f)
(check-equal? (occurs-bound? 'y (parse-lc-exp E4)) #f)
(check-equal? (occurs-bound? 'y (parse-lc-exp E3)) #t)
(check-equal? (occurs-bound? 'x (parse-lc-exp E4)) #t)
