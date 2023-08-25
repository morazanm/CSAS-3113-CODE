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

    (expression ("%lexvar" number number) nameless-var-exp)
    
    (expression("-" "(" expression "," expression ")")diff-exp)
    
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression) if-exp)
    
    (expression 
     ("let" (arbno identifier "=" expression) "in" expression) let-exp)

    (expression ("%let" (arbno expression) "in" expression) nameless-let-exp)
    
    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
               "in" expression) letrec-exp)

    (expression
     ("%letrec" (arbno expression) "in" expression) nameless-letrec-exp)
    
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)

    (expression ("%nameless-proc" "(" expression ")") nameless-proc-exp)
    
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

#|

A rib is a (listof expval)

An environment is a (listof rib)

|#

(define (environment? e)
  (list-of (list-of expval?)))

;;  --> environment
;; Purpose: Build the empty env
(define (empty-env) '())

;; (listof expval) environment --> environment
;; Purpose: Build an environment from given expvals and env
(define (extend-env vals env) (cons vals env))

;; environment natnum natnum --> expval throws error
;; Purpose: Return expval at given lexical address in given env
(define (apply-env env depth pos)
  (if (empty? env)
      (eopl:error 'apply-env "No binding for lexical address: ~s ~s" depth pos)
      (list-ref (list-ref env depth) pos)))



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
;; Purpose: Throw expval extraction error
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
   (body expression?)
   (envv voenv?)))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
               (value-of exp1 (empty-env)))))

;; exp env -> expval
;; Purpose: Evaluate given expression in given environment
(define (value-of exp env)
  (cases expression exp
    
    (const-exp (num) (num-val num))

    (true-exp () (bool-val #t))

    (false-exp () (bool-val #f))
    
    (var-exp (var) (apply-env env var))

    (nameless-var-exp (d p) (apply-env env d p))
    
    (diff-exp (exp1 exp2)
              (let ((num1 (expval2num (value-of exp1 env)))
                    (num2 (expval2num (value-of exp2 env))))
                (num-val (- num1 num2))))
    
    (zero?-exp (exp1)
               (let ((val1 (expval2num (value-of exp1 env))))
                 (if (zero? val1)
                     (bool-val #t)
                     (bool-val #f))))
    
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
              (if (expval2bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    
    (let-exp (vars exps body)       
             (let [(vals (map (lambda (e) (value-of e env)) exps))]
               (value-of body
                         (foldr (lambda (binding acc)
                                  (extend-env (car binding)
                                              (cadr binding)
                                              acc))
                                env
                                (map (lambda (var val) (list var val))
                                     vars
                                     vals)))))

    (nameless-let-exp (exps body)
                      (let [(vals (map (lambda (e) (value-of e env)) exps))]
                        (value-of body
                                  (extend-env vals env))))

    (proc-exp (params body)
              (proc-val (procedure params body (vector env))))

    (nameless-proc-exp (body)
                       (proc-val (procedure body (vector env))))
    
    (call-exp (rator rands)
              (let [(proc (expval2proc (value-of rator env)))
                    (args (map (lambda (rand) (value-of rand env)) rands))]
                (apply-procedure proc args)))

    (letrec-exp (names params bodies letrec-body)
                (value-of letrec-body (mk-letrec-env names params bodies env)))

    (nameless-letrec-exp (bodies letrec-body)
                         (value-of letrec-body (mk-letrec-env bodies env)))))

;; (listof expression) environment --> environment
;; Purpose: Add the proc-vals for the given procedures in the given environment
(define (mk-letrec-env bodies env)
  (let* [(temp-proc-vals (map (lambda (b)
                                (proc-val (procedure b (vector (empty-env)))))
                              bodies))
         (new-env (extend-env temp-proc-vals env))]
    (begin
      (for-each (lambda (p)
                  (cases proc p
                    (procedure (b ve)
                               (vector-set! ve 0 new-env))))
                (map (lambda (p) (expval2proc p))
                     temp-proc-vals))
      new-env)))
                           

;; apply-procedure : proc (listof expval) -> expval
;; Purpose: Apply the given procedure to the given values
(define (apply-procedure f vals)
  (cases proc f
    (procedure (body envv)
               (let [(saved-env (vector-ref envv 0))]
                 (value-of body
                           (extend-env vals saved-env))))))

;; Nameless Translation

;; static environment
(define-datatype senv senv?
  (empty-senv)
  (extend-senv (vars (list-of symbol?))
               (esenv senv?)))

(define (apply-senv ev var)
  (cases senv ev
    (empty-senv () (eopl:error )apply-senv "Free variabe: ~s" var)
    (extend-senv (vrs seenv)
                 (let [(lst (member var vrs))]
                   (if (false? lst)
                       (let [(res (apply-senv seenv var))]
                         (list (add1 (first res)) (second res)))
                       (list 0 (- (length vrs) (length lst))))))))
                
;; program --> program
;; Purpose: Change var references to lexical-addresses
(define (translate-program-nameless p)
  (cases program p
    (a-program (exp)
               (a-program (translate-exp-nameless exp (empty-senv))))))

;; expression --> expression
;; Purpose: Change var references to lexical-addresses
(define (translate-exp-nameless exp senv)
  (cases expression exp
    (var-exp (var)
             (let [(lex-addr (apply-senv senv var))]
               (nameless-var-exp (first lex-addr) (second lex-addr))))
    (diff-exp (exp1 exp2)
              (diff-exp (translate-exp-nameless exp1 senv)
                        (translate-exp-nameless exp2 senv)))
    (zero?-exp (exp1)
               (zero?-exp (translate-exp-nameless exp1 senv)))
    (if-exp (exp1 exp2 exp3)
            (if-exp
             (translate-exp-nameless exp1 senv)
             (translate-exp-nameless exp2 senv)
             (translate-exp-nameless exp3 senv)))
    (let-exp (vars exps body)
             (nameless-let-exp
              (map (lambda (e) (translate-exp-nameless e senv))
                   exps)
              (translate-exp-nameless body (extend-senv vars senv))))
    (proc-exp (params body)
              (nameless-proc-exp (translate-exp-nameless body (extend-senv params senv))))
    (call-exp (rator rands)
              (call-exp
               (translate-exp-nameless rator senv)
               (map (lambda (e) (translate-exp-nameless e senv))
                    rands)))
    (letrec-exp (names params bodies body)
                (nameless-letrec-exp
                 (map (lambda (e ps)
                        (translate-exp-nameless
                         e
                         (extend-senv ps (extend-senv names senv))))
                      bodies
                      params)
                 (translate-exp-nameless body (extend-senv names senv))))
    (else exp)))

;;;;;;   EVALUATION WRAPPERS

;; string -> a-program
;; Purpose: Parse the given extended LC-program
(define (parse p) (scan&parse p))

;; string -> ExpVal
;; Purpose: Evaluate the given extended LC-program
(define (eval string)
  (value-of-program (translate-program-nameless (parse string))))

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












