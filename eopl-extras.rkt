#lang racket

(provide filter ormap andmap build-list foldr foldl add1 sub1 first second third
         fourth fifth sixth seventh eighth ninth tenth rest displayln display
         vector-of for-each false? empty? take drop stream-cons stream-ref
         stream-map λ void format generate-symbol last drop-right append-map)

(define (vector-of V f)
  (define (helper low high)
    (if (> low high)
        #t
        (and (f (vector-ref V low))
             (helper (add1 low) high))))
  (helper 0 (sub1 (vector-length V))))

;; symbol --> symbol
;; Purpose: Generate a new symbol
(define (generate-symbol s)
  (let ((new-symb (string->symbol (symbol->string (gensym (string->symbol (string-append (symbol->string s) "-")))))))
    new-symb))