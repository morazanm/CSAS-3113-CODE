#lang eopl

(provide empty-env extend-env apply-env)

;;  --> env
;; Purpose: Construct the empty env
(define (empty-env) '(empty-env))

;; symbol X env --> env
;; Purpose: Add a binding to the given env
(define (extend-env var val e)
  (list 'extend-env var val e))

;; env symbol --> X
;; Purpose: Get the value of given var in given env
(define (apply-env e var)
  (cond [(eq? (car e) 'empty-env)
         (eopl:error 'apply-env "No binding for ~s" var)]
        [(eq? (cadr e) var) (caddr e)]
        [else (apply-env (cadddr e) var)]))

