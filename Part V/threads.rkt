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
    
    (expression("-" "(" expression "," expression ")") diff-exp)
    
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
    
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression ("set" identifier "=" expression)  set-exp)

    (expression ("print" "(" expression ")" )  print-exp)

    (expression ("spawn" "(" expression ")") spawn-exp)

    (expression ("mutex" "(" ")")  mutex-exp)

    (expression ("wait" "(" expression ")") wait-exp)

    (expression ("signal" "(" expression ")") signal-exp)

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
   (bval reference?)
   (saved-env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (var val saved-env)
                (if (eqv? search-sym var)
                    val
                    (apply-env saved-env search-sym)))))

;;;;;;; The store ;;;;;;;;;;;;;;;;;;;;

;; reference? : RacketVal -> Bool
(define (reference? v) (integer? v))


;; the-store: a Racket variable containing the current state of the
;; store.  Initially set to a dummy value.
(define the-store 'uninitialized)


;; empty-store : () -> store
(define (empty-store) '())

;; initialize-store! : () -> store
;; usage: (initialize-store!) sets the-store to the empty-store
(define (initialize-store!) (set! the-store (empty-store)))


;; newref : expval -> ref
(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val))) 
    next-ref))

;; deref : ref -> expval
(define (deref ref) (list-ref the-store ref))

;; setref : ref expval -> expval
(define (setref! ref new-expval)
  (set! the-store (append (take the-store ref)
                          (list new-expval)
                          (drop the-store (add1 ref)))))



;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (mutex-val
   (mutex mutex?)))

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

(define expval2mutex
  (lambda (v)
    (cases expval v
      (mutex-val (m) m)
      (else (expval-extractor-error 'mutex v)))))

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

;;;;;; Queue Interface

(define (empty-queue) '())

(define empty-queue? null?)

(define (enqueue q val) (append q (list val)))

(define (dequeue q f) (f (car q) (cdr q)))

;;;;;; State variables (registers)

(define the-ready-queue    'uninitialized)

(define the-final-answer   'uninitialized)

(define the-max-time-slice 'uninitialized)

(define the-time-remaining 'uninitialized)

;;;;;; Scheduler implementation

;; initialize-scheduler! : Int -> void
(define (initialize-scheduler! ticks)
  (begin
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))

;; thread -> (void)
(define (place-on-ready-queue! th)
  (set! the-ready-queue (enqueue the-ready-queue th)))

;; expval -> (void)
(define (set-final-answer! val) (set! the-final-answer val))

;;  -> Bool
(define (time-expired?) (zero? the-time-remaining))

;;  -> Unspecified
(define (decrement-timer!)
  (set! the-time-remaining (- the-time-remaining 1)))

;;  -> FinalAnswer
(define (run-next-thread)
  (if (empty-queue? the-ready-queue)
      the-final-answer
      (dequeue the-ready-queue
               (lambda (first-ready-thread other-ready-threads)
                 (set! the-ready-queue other-ready-threads)
                 (set! the-time-remaining the-max-time-slice)
                 (first-ready-thread)))))

;;;;;; Semaphores

(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed? reference?)      ;; ref to bool closed or open
   (ref-to-wait-queue reference?))) ;; waiting queue is ref to (listof thread)

(define (new-mutex)
  (a-mutex (newref #f)   (newref '())))

;; mutex thread -> expval
;; Purpose: waits for mutex to be open.
(define (wait-for-mutex m th)
  (cases mutex m
    (a-mutex (ref-to-closed? ref-to-wait-queue)
             (cond [(deref ref-to-closed?)
                    (begin
                      (setref! ref-to-wait-queue
                               (enqueue (deref ref-to-wait-queue) th))
                      (run-next-thread))]
                   [else
                    (begin
                      (setref! ref-to-closed? #t)
                      (th))]))))

;; mutex thread -> expval
;; Purpose: To signal the given mutex and run a waiting thread if any
(define (signal-mutex m th)
  (cases mutex m
    (a-mutex (ref-to-closed? ref-to-wait-queue)
             (let [(closed? (deref ref-to-closed?))
                   (wait-queue (deref ref-to-wait-queue))]
               (if closed?
                   (begin
                     (if (empty? wait-queue)
                         (setref! ref-to-closed? #f)
                         (dequeue wait-queue
                                  (lambda (first-waiting-th other-waiting-ths)
                                    (begin
                                      (place-on-ready-queue!
                                       first-waiting-th)
                                      (setref!
                                       ref-to-wait-queue
                                       other-waiting-ths)))))
                     (th)) ;;error in book: Page 190, Fig 5.22
                   (th))))))


;;;;;; Continuations

(define-datatype continuation cont?
  (zero?-cont (k cont?))
  (let-cont (vars (list-of symbol?))
            (env environment?)
            (body expression?)
            (saved-cont cont?))
  (eval-rhss-cont1 (exps (list-of expression?))
                   (env environment?)
                   (k cont?))

  (eval-rhss-cont2 (fval expval?)
                   (k cont?))
  (letrec-cont (letrec-body expression?)
               (saved-cont cont?))
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
                    (k cont?))
  (set-cont (loc reference?) (k cont?))
  (print-cont (k cont?))
  (eval-exprs-cont (exprs (list-of expression?))
                   (env environment?)
                   (k cont?))
  (eval-last-exprs-cont (k cont?))
  (spawn-cont (saved-cont cont?))
  (wait-cont (saved-cont cont?))
  (signal-cont (saved-cont cont?))
  (end-main-thread-cont)           
  (end-subthread-cont))


;; continuation expval --> expval
;; Purpose: Apply the given cont to the given value and return the final answer
(define (apply-cont k val)
  (if (time-expired?)
      (begin
        (place-on-ready-queue! (lambda () (apply-cont k val)))
        (run-next-thread))
      (begin
        (decrement-timer!)
        (cases continuation k
    
          (zero?-cont (cont)
                      (if (zero? (expval2num val))
                          (apply-cont cont (bool-val #t))
                          (apply-cont cont (bool-val #f))))

          (if-cont (exp2 exp3 env saved-cont)
                   (if (expval2bool val)
                       (value-of/k exp2 env saved-cont)
                       (value-of/k exp3 env saved-cont)))

          (let-cont (vars env body saved-cont)
                    (let [(new-env (foldr (lambda (var val acc)
                                            (extend-env var (newref val) acc))
                                          env
                                          vars
                                          val))]
                      ;(display (format "new-env: ~s\n\n" new-env))
                      (value-of/k
                       body
                       new-env
                       saved-cont)))

          (eval-rhss-cont1 (exps env saved-cont)
                           (eval-rhss exps
                                      env
                                      (eval-rhss-cont2 val saved-cont)))

          (eval-rhss-cont2 (fval k)
                           (apply-cont k (cons fval val)))

          (letrec-cont (letrec-body saved-cont)
                       (value-of/k letrec-body val saved-cont))

          (diff-cont1 (exp2 env saved-cont)
                      (value-of/k exp2 env (diff-cont2 val saved-cont)))

          (diff-cont2 (val1 saved-cont)
                      (apply-cont saved-cont
                                  (num-val (- (expval2num val1)
                                              (expval2num val)))))
          (rator-cont (rands env saved-cont)
                      (eval-rands/k rands env (rands-cont val saved-cont)))

          (rands-cont (rator saved-cont)
                      (apply-procedure/k (expval2proc rator) val saved-cont))
          (eval-rands-cont1 (rands env saved-cont)
                            (eval-rands/k rands env (eval-rands-cont2 val saved-cont)))
          (eval-rands-cont2 (first-rand k)
                            (apply-cont k (cons first-rand val)))
          (set-cont (loc k)
                    (begin
                      (setref! loc val)
                      (apply-cont k (num-val 31))))
          (print-cont (k)
                      (begin
                        (display (format "~s\n" val))
                        (apply-cont k (num-val -1))))
          (eval-exprs-cont (exprs env k) (eval-exprs exprs env k))
          (eval-last-exprs-cont (k) (apply-cont k val))
          (end-subthread-cont () (run-next-thread))
          (end-main-thread-cont ()
                                ;(displayln "END")
                                (set-final-answer! val)
                                (run-next-thread))
          (spawn-cont (saved-cont)
                      (let ((proc1 (expval2proc val)))
                        (begin
                          (place-on-ready-queue!
                           (lambda ()
                             (apply-procedure/k proc1
                                                (list (num-val 28))
                                                (end-subthread-cont))))
                          (apply-cont saved-cont (num-val 73)))))
          (wait-cont (saved-cont)
                     (wait-for-mutex
                      (expval2mutex val)
                      (lambda () (apply-cont saved-cont (num-val 52)))))
          (signal-cont (saved-cont)
                       (signal-mutex
                        (expval2mutex val)
                        (lambda () (apply-cont saved-cont (num-val 53)))))
          ))))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : program -> expval
(define (value-of-program timeslice pgm)
  (initialize-store!)
  (initialize-scheduler! timeslice)
  (cases program pgm
    (a-program (exp1)
               (begin
                 (value-of/k
                  exp1
                  (empty-env)
                  (end-main-thread-cont))
                 the-final-answer))))

;; expression environment continuation -> expval
(define (value-of/k exp env k)
  ;(display (format "evaluating: ~s\n\n" exp))
  (cases expression exp
    
    (const-exp (num) (apply-cont k (num-val num)))

    (true-exp () (apply-cont k (bool-val #t)))

    (false-exp () (apply-cont k (bool-val #f)))
    
    (var-exp (var)
             (apply-cont k (deref (apply-env env var))))

    (proc-exp (params body)
              (apply-cont k (proc-val (procedure params body (vector env)))))

    (zero?-exp (exp1)
               (value-of/k exp1 env (zero?-cont k)))
    
    (diff-exp (exp1 exp2)
              (value-of/k exp1 env (diff-cont1 exp2 env k)))
    
    (if-exp (exp1 exp2 exp3)
            (value-of/k exp1 env (if-cont exp2 exp3 env k)))
    
    (let-exp (vars exps body)
             (eval-rhss exps env (let-cont vars env body k)))
    
    (call-exp (rator rands)
              (value-of/k rator env (rator-cont rands env k)))

    (letrec-exp (names params bodies letrec-body)
                (mk-letrec-env/k names params bodies env (letrec-cont letrec-body k)))
    
    (set-exp (var exp1)
             (value-of/k exp1 env (set-cont (apply-env env var) k)))

    (begin-exp (exp exps) (eval-exprs (cons exp exps) env k))

    (print-exp (exp) (value-of/k exp env (print-cont k)))
    
    (spawn-exp (exp)
               ;(display (format "Spawnning...\n\n"))
               (value-of/k exp env (spawn-cont k)))

    (mutex-exp ()
               (apply-cont k (mutex-val (new-mutex))))

    (wait-exp (exp)
              (value-of/k exp env (wait-cont k)))

    (signal-exp (exp)
                (value-of/k exp env (signal-cont k)))))

(define (eval-rhss exps env k)
  (if (empty? exps)
      (apply-cont k '())
      (value-of/k (first exps) env (eval-rhss-cont1 (rest exps) env k))))

;; (listof symbol) (listof (listof symbol)) (listof expression) environment continuation --> environment
;; Purpose: Add the proc-vals for the given procedures in the given environment
(define (mk-letrec-env/k names params bodies env k)
  (let* [(temp-proc-vals (map (lambda (p b)
                                (proc-val (procedure p b (vector (empty-env)))))
                              params
                              bodies))
         (new-env (foldl (lambda (name proc env)
                           (extend-env name
                                       (newref proc) 
                                       env))
                         env
                         names
                         temp-proc-vals))]
    (begin
      (for-each (lambda (p)
                  (cases proc p
                    (procedure (p b ve)
                               (vector-set! ve 0 new-env))))
                (map (lambda (p) (expval2proc p))
                     temp-proc-vals))
      (apply-cont k new-env))))

;; (listof expr) environment continutation --> expval
(define (eval-exprs exprs env k)
  (if (null? (rest exprs))
      (value-of/k (first exprs) env (eval-last-exprs-cont k))
      (value-of/k (first exprs) env (eval-exprs-cont (rest exprs) env k))))

  
;; (listof expression) environment continuation --> expval
;; Purpose: Evaluate the given list of exprs and apply the given cont
(define (eval-rands/k rands env k)
  (if (null? rands)
      (apply-cont k '())
      (value-of/k (car rands)
                  env
                  (eval-rands-cont1 (cdr rands) env k))))

;; (listof symbol) (listof (listof symbol)) (listof expression) environment --> environment
;; Purpose: Add the proc-vals for the given procedures in the given environment
(define (mk-letrec-env names params bodies env)
  (let* [(temp-proc-vals (map (lambda (p b)
                                (proc-val (procedure p b (vector (empty-env)))))
                              params
                              bodies))
         (new-env (foldl (lambda (name proc env)
                           (extend-env name
                                       (newref proc)
                                       env))
                         env
                         names
                         temp-proc-vals))]
    (begin
      (for-each (lambda (p)
                  (cases proc p
                    (procedure (p b ve)
                               (vector-set! ve 0 new-env))))
                (map (lambda (p) (expval2proc p))
                     temp-proc-vals))
      new-env)))
                           

;; proc (listof expval) continuation -> expval
;; Purpose: Apply the given procedure to the given values
(define (apply-procedure/k f vals k)
  (cases proc f
    (procedure (params body envv)
               (let [(saved-env (vector-ref envv 0))]
                 (value-of/k body
                             (foldr (lambda (binding acc)
                                      (extend-env (car binding)
                                                  (newref (cadr binding))
                                                  acc))
                                    saved-env
                                    (map (lambda (p v) (list p v))
                                         params
                                         vals))
                             k)))))

;;;;;;   EVALUATION WRAPPERS

;; string -> a-program
;; Purpose: Parse the given extended LC-program
(define (parse p) (scan&parse p))

(define TIMESLICE 5) ;5 or 20 or 100 to illustrate (especially with noisy & buffer examples)

;; string -> ExpVal
;; Purpose: Evaluate the given extended LC-program
(define (eval string)
  (value-of-program TIMESLICE (parse string)))

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

(check-equal? (eval "let a = 3
                     in let p = proc (x) set x = 4
                        in begin 
                             (p a); 
                             a 
                           end")
              (num-val 3))

(check-equal? (eval "let x = 0
                     in letrec f (x) = set x = -(x, 1)
                               g (a) = set x = -(x, 2)
                        in begin
                             (f x); % remember: call by value, not reference
                             (g x); % remember: call by value, not reference
                             x
                           end")
              (num-val -2))

(check-equal? (eval "letrec noisy (n) = if zero?(n)
                                        then 0
                                        else begin
                                               print(n);
                                               (noisy -(n, 1))
                                             end
                     in  begin
                           spawn(proc (d) (noisy 5));
                           spawn(proc (d) (noisy 10));
                           print(100);
                           33
                         end")
              (num-val 33))
(newline)

(check-equal? (eval "let x = 0 
                     in let mut = mutex() 
                        in let incr_x = proc (id) proc (dummy) 
                                          begin
                                            set x = -(x, -1);
                                            print(x) 
                                          end 
                           in begin 
                               spawn((incr_x 100));
                               spawn((incr_x 200)); 
                               spawn((incr_x 300))
                              end")
              (num-val 73))
(newline)

(check-equal? (eval "let x = 0 
                     in let mut = mutex() 
                        in let incr_x = proc (id) proc (dummy) 
                                          begin
                                            wait(mut);
                                            set x = -(x, -1);
                                            print(x);
                                            signal(mut) 
                                          end 
                           in begin 
                               spawn((incr_x 100));
                               spawn((incr_x 200)); 
                               spawn((incr_x 300))
                              end")
              (num-val 73))


(check-equal?
 (eval "let buffer = 0
        in let producer = proc (n) 
                            letrec 
                              waitloop(k) = if zero?(k) 
                                            then set buffer = n
                                            else begin
                                                   print(-(k,-200));
                                                   (waitloop -(k,1))
                                                 end
                              in (waitloop 5)
           in let consumer = proc (d)
                               letrec
                                 busywait (k) = if zero?(buffer)
                                                then begin
                                                       print(-(k,-100));
                                                       (busywait -(k,-1))
                                                     end
                                                else buffer
                               in (busywait 0)
              in begin
                   spawn(proc (d) (producer 44));
                   (consumer 86)
                 end")
 (num-val 44))
(newline)




