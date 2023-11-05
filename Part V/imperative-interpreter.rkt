#lang eopl

(require rackunit "../eopl-extras.rkt")

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    
    (comment ("%" (arbno (not #\newline))) skip)
    
    (identifier
     (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    
    (number (digit (arbno digit)) number)
    
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)

    (expression ("true") true-exp)

    (expression ("false") false-exp)

    (expression (identifier) var-exp)
    
    (expression("-" "(" expression "," expression ")")diff-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    
    (expression 
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
               "in" expression) letrec-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    
    (expression ("(" expression (arbno expression) ")") call-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;    ENVIRONMENT

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val saved-env)
                (if (eqv? search-sym var)
                    val
                    (apply-env saved-env search-sym)))))



;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?)))

;;; extractors:

;; expval2num : ExpVal -> Int
;; expval --> Int throws error
;; Purpose: Extract number from given expval
(define (expval2num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num-val v))))

;; expval --> Bool throws error
;; Purpose: Extract Boolean from given expval
(define (expval2bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool-val v))))

;; expval --> proc throws error
;; Purpose: Extract proc from given expval
(define (expval2proc v)
  (cases expval v
    (proc-val (proc) proc)
    (else (expval-extractor-error 'proc-val v))))

;; symbol expval --> throws error
(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, given ~s"
              variant value))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

;; Any --> Boolean
;; Purpose: Determine if given value is a vector with a single environment
(define (voenv? penv)
  (and (vector? penv)
       (= (vector-length penv) 1)
       (environment? (vector-ref penv 0))))

(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (body expression?)
   (envv voenv?)))

;;; The continuations

(define-datatype continuation cont?
  (end-cont)
  (zero?-cont (k cont?))
  (letrec-cont (letrec-body expression?)
               (saved-cont cont?))
  (let1-cont (vrs (list-of symbol?))
             (b expression?)
             (e environment?)
             (k cont?))
  (let2-cont (b expression?)
             (k cont?))
  (if-cont (e2 expression?)
           (e3 expression?)
           (e environment?)
           (k cont?))
  (diff-cont1 (e2 expression?)
              (e environment?)
              (k cont?))
  (diff-cont2 (v1 expval?)
              (k cont?))
  (rator-cont (rnds (list-of expression?))
              (e environment?)
              (k cont?))
  (rands-cont (operator expval?)
              (k cont?))
  (eval-rands-cont1 (rands (list-of expression?))
                    (e environment?)
                    (k cont?))
  (eval-rands-cont2 (farg expval?)
                    (k cont?)))

;;; The registers

(define exp (void))
(define env (void))
(define cont (void))
(define val (void))
(define proc1 (void))
(define rands (void))
(define vars (void))
(define vals (void))
(define letrec-names (void))
(define letrec-params (void))
(define letrec-bodies (void))
  

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; continuation expval --> expval
;; Purpose: Apply the given cont to the given value and return the final answer
(define (apply-cont)
  ;;(define d (displayln (format "Applying: ~s to ~s\n\n" cont val)))
  
  (cases continuation cont
    (end-cont () val)
    
    (zero?-cont (saved-cont)
                (if (zero? (expval2num val))
                    #;(apply-cont saved-cont (bool-val #t))
                    (begin
                      (set! val (bool-val #t))
                      (set! cont saved-cont)
                      (apply-cont))
                    #;(apply-cont cont (bool-val #f))
                    (begin
                      (set! val (bool-val #f))
                      (set! cont saved-cont)
                      (apply-cont))))

    (let1-cont (the-vars body saved-env saved-cont)
               #;(create-let-lenv vars val saved-env (let2-cont body saved-cont))
               (begin
                 (set! vars the-vars)
                 (set! vals val) ;; val is the list of evaluated RHSs
                 (set! env saved-env)
                 (set! cont (let2-cont body saved-cont))
                 (create-let-lenv)))

    (let2-cont (body saved-cont)
               #;(value-of/k body val saved-cont)
               (begin
                 (set! exp body)
                 (set! env val) ;; val is an env
                 (set! cont saved-cont)
                 (value-of/k)))

    (if-cont (exp2 exp3 saved-env saved-cont)
             (if (expval2bool val)
                 #;(value-of/k exp2 saved-env saved-cont)
                 (begin
                   (set! exp exp2)
                   (set! env saved-env)
                   (set! cont saved-cont)
                   (value-of/k))
                 #;(value-of/k exp3 env saved-cont)
                 (begin
                   (set! exp exp3)
                   (set! cont saved-cont)
                   (value-of/k))))

    (letrec-cont (letrec-body saved-cont)
                 #;(value-of/k letrec-body val saved-cont)
                 (begin
                   (set! exp letrec-body)
                   (set! env val) ;; val is an env
                   (set! cont saved-cont)
                   (value-of/k)))

    (diff-cont1 (exp2 saved-env saved-cont)
                #;(value-of/k exp2 saved-env (diff-cont2 val saved-cont))
                (begin
                  (set! exp exp2)
                  (set! env saved-env)
                  (set! cont (diff-cont2 val saved-cont))
                  (value-of/k)))

    (diff-cont2 (val1 saved-cont)
                #;(apply-cont saved-cont (num-val (- (expval2num val1)
                                                     (expval2num val))))
                (begin
                  (set! cont saved-cont)
                  (set! val (num-val (- (expval2num val1) (expval2num val))))
                  (apply-cont)))
    
    (rator-cont (saved-rands saved-env saved-cont)
                #;(eval-rands/k saved-rands saved-env (rands-cont val saved-cont))
                (begin
                  (set! rands saved-rands)
                  (set! env saved-env)
                  (set! cont (rands-cont val saved-cont))
                  (eval-rands/k)))

    (rands-cont (rator saved-cont)
                #;(apply-procedure/k (expval2proc rator) val saved-cont)
                (begin
                  (set! proc1 (expval2proc rator))
                  (set! vals val) ;; val is the list of evaluated args
                  (set! cont saved-cont)
                  (apply-procedure/k)))
    
    (eval-rands-cont1 (saved-rands saved-env saved-cont)
                      #;(eval-rands/k saved-rands saved-env (eval-rands-cont2 val saved-cont))
                      (begin
                        (set! rands saved-rands)
                        (set! env saved-env)
                        (set! cont (eval-rands-cont2 val saved-cont))
                        (eval-rands/k)))
    
    (eval-rands-cont2 (first-rand saved-cont)
                      #;(apply-cont saved-cont (cons first-rand val))
                      (begin
                        (set! cont saved-cont)
                        (set! val (cons first-rand val))
                        (apply-cont)))))
                      
;; (listof expression) environment continuation --> expval
;; Purpose: Evaluate the given list of exprs and apply the given cont
(define (eval-rands/k)
  (if (null? rands)
      #;(apply-cont k '())
      (begin
        (set! val '())
        (apply-cont))
      #;(value-of/k (car rands) env (eval-rands-cont1 (cdr rands) env k))
      (begin
        (set! exp (car rands))
        (set! cont (eval-rands-cont1 (cdr rands) env cont))
        (value-of/k))))

;; (listof symbol) (listof expval) environment continuation --> expval
;; Purpose: Apply the given cont to the evn created usingthe given
;;          variables, expvals, and environment
(define (create-let-lenv)
  ;;(define d (displayln (format "env in create-let-env = ~s\n" env)))
  
  (if (empty? vars)
      #;(apply-cont k env)
      (begin
        (set! val env)
        (apply-cont))
      #;(create-let-lenv (cdr vars)
                         (cdr vals)
                         (extend-env (car vars) (car vals) env)
                         k)
      (begin
        (set! env (extend-env (car vars) (car vals) env)) ;; beware of dependencies between registers
        (set! vars (cdr vars))
        (set! vals (cdr vals))
        (create-let-lenv))))
    
  
;; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
               (begin
                 (set! exp exp1)
                 (set! env (empty-env))
                 (set! cont (end-cont))
                 (value-of/k)))))

;; expression environment continuation -> expval
(define (value-of/k)
  (cases expression exp
    
    (const-exp (num)
               #;(apply-cont k (num-val num))
               (begin
                 (set! val (num-val num))
                 (apply-cont)))

    (true-exp ()
              #;(apply-cont k (bool-val #t))
              (begin
                (set! val (bool-val #t))
                (apply-cont)))

    (false-exp ()
               #;(apply-cont k (bool-val #f))
               (begin
                 (set! val (bool-val #f))
                 (apply-cont)))
    
    (var-exp (var)
             #;(apply-cont k (apply-env env var))
             (begin
               (set! val (apply-env env var))
               (apply-cont)))

    (proc-exp (params body)
              #;(apply-cont k (proc-val (procedure params body (vector env))))
              (begin
                (set! val (proc-val (procedure params body (vector env))))
                (apply-cont)))

    (zero?-exp (exp1)
               #;(value-of/k exp1 env (zero?-cont k))
               (begin
                 (set! exp exp1)
                 (set! cont (zero?-cont cont))
                 (value-of/k)))
    
    (diff-exp (exp1 exp2)
              #;(value-of/k exp1 env (diff-cont1 exp2 env k))
              (begin
                (set! exp exp1)
                (set! cont (diff-cont1 exp2 env cont))
                (value-of/k)))
    
    (if-exp (exp1 exp2 exp3)
            #;(value-of/k exp1 env (if-cont exp2 exp3 env k))
            (begin
              (set! exp exp1)
              (set! cont (if-cont exp2 exp3 env cont))
              (value-of/k)))
    
    (let-exp (vars exps body)
             #;(eval-rands/k exps env (let1-cont vars body env k))
             (begin
               (set! rands exps)
               (set! cont (let1-cont vars body env cont))
               (eval-rands/k)))
    
    (call-exp (rator rands)
              #;(value-of/k rator env (rator-cont rands env k))
              (begin
                (set! exp rator)
                (set! cont (rator-cont rands env cont))
                (value-of/k)))

    (letrec-exp (names params bodies letrec-body)
                #;(mk-letrec-env/k names params bodies env (letrec-cont letrec-body k))
                (begin
                  (set! letrec-names names)
                  (set! letrec-params params)
                  (set! letrec-bodies bodies)
                  (set! cont (letrec-cont letrec-body cont))
                  (mk-letrec-env/k)))))

;; (listof symbol) (listof (listof symbol)) (listof expression) environment continuation --> environment
;; Purpose: Add the proc-vals for the given procedures in the given environment
(define (mk-letrec-env/k)
  (let* [(temp-proc-vals (map (lambda (p b)
                                (proc-val (procedure p b (vector (empty-env)))))
                              letrec-params
                              letrec-bodies))
         (new-env (foldl (lambda (name proc env)
                           (extend-env name
                                       proc
                                       env))
                         env
                         letrec-names
                         temp-proc-vals))]
    (begin
      (for-each (lambda (p)
                  (cases proc p
                    (procedure (p b ve)
                               (vector-set! ve 0 new-env))))
                (map (lambda (p) (expval2proc p))
                     temp-proc-vals))
      #;(apply-cont k new-env)
      (begin
        (set! val new-env)
        (apply-cont)))))
                           

;; proc (listof expval) continuation -> expval
;; Purpose: Apply the given procedure to the given values
(define (apply-procedure/k)
  (cases proc proc1
    (procedure (params body envv)
               (let [(saved-env (vector-ref envv 0))]
                 #;(value-of/k body
                               (foldr (lambda (binding acc)
                                        (extend-env (car binding)
                                                    (cadr binding)
                                                    acc))
                                      saved-env
                                      (map (lambda (p v) (list p v))
                                           params
                                           vals))
                               k)
                 (begin
                   (set! exp body)
                   (set! env (foldr (lambda (binding acc)
                                      (extend-env (car binding)
                                                  (cadr binding)
                                                  acc))
                                    saved-env
                                    (map (lambda (p v) (list p v))
                                         params
                                         vals)))
                   (value-of/k))))))

;;;;;;   EVALUATION WRAPPERS

;; string -> a-program
;; Purpose: Parse the given extended LC-program
(define (parse p) (scan&parse p))

;; string -> ExpVal
;; Purpose: Evaluate the given extended LC-program
(define (eval string)
  (value-of-program (parse string)))

;;;;; EXAMPLES OF EVALUATION

(check-equal? (eval "if zero?(1) then 1 else 2")
              (num-val 2))

(check-equal? (eval "-(15, 10)")
              (num-val 5))

(check-equal? (eval "let x = 10 in if zero?(-(x, x)) then x else 2")
              (num-val 10))

(check-equal? (eval "let decr = proc (a) -(a, 1) in (decr 30)")
              (num-val 29))

(check-equal? (eval "( proc (g) (g 30) proc (y) -(y, 1))")
              (num-val 29))

(check-equal? (eval "let x = 200
                     in let f = proc (z) -(z, x) 
                        in let x = 100 
                           in let g = proc (z) -(z, x) 
                              in -((f 1), (g 1))")
              (num-val -100))

(check-equal? (eval "let sum = proc (x) proc (y) -(x, -(0, y)) in ((sum 3) 4)")
              (num-val 7))

(check-equal? (eval "let sum = proc (x) proc (y) -(x, -(0, y))
                     in letrec sigma (n) = if zero?(n)
                                           then 0
                                           else ((sum n) (sigma -(n, 1)))
                        in (sigma 5)")
              (num-val 15))

(check-equal? (eval "letrec even(n) = if zero?(n)
                                      then zero?(n)
                                      else if zero?(-(n, 1))
                                           then zero?(n)
                                           else (even -(n, 2))
                     in (even 501)")
              (bool-val #f))













