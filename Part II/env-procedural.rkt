#lang eopl

(require rackunit)

(define (empty-env)
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define (extend-env var val e)
  (lambda (search-var)
    (cond [(eqv? search-var var) val]
          [else (apply-env e search-var)])))

(define (apply-env e var) (e var))