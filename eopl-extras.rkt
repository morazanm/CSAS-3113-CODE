#lang racket

(provide ormap andmap build-list foldr foldl add1 sub1 first second third
         fourth fifth sixth seventh eighth ninth tenth displayln display
         vector-of for-each false? empty? take drop stream-cons stream-ref
         stream-map λ format)

(define (vector-of V f)
  (define (helper low high)
    (if (> low high)
        #t
        (and (f (vector-ref V low))
             (helper (add1 low) high))))
  (helper 0 (sub1 (vector-length V))))