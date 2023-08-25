#lang eopl
(require rackunit)

#|

<slist>	::= ({<s-exp>}*)
<sexp>	::= <symbol> | <s-list>

|#

;; slist --> slist
;; Purpose: Substitute old with new in given slist
(define (subst-slist old new an-slist)
  (map (lambda (s) (subst-sexp old new s)) an-slist))

(define (subst-sexp old new a-sexp)
  (if (symbol? a-sexp)
      (if (eq? old a-sexp)
          new
          a-sexp)
      (subst-slist old new a-sexp)))

(check-equal? (subst-slist 'a 'b '()) '())
(check-equal? (subst-slist 'c 'c '(b c c c)) '(b c c c))
(check-equal? (subst-slist 'a 'z '(a (b x a) (f (g a a) b)))
              '(z (b x z) (f (g z z) b)))

(check-equal? (subst-sexp 'a 'b 'a) 'b)
(check-equal? (subst-sexp 'b 'z 'c) 'c)
(check-equal? (subst-sexp 'i 'j '(u h (i (j h s (i i i) s) y)))
              '(u h (j (j h s (j j j) s) y)))