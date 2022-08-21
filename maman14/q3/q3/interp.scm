(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
(proc-exp (ids defs body)
          (proc-val (procedure ids defs body env)))

(call-exp (rator parms exps)
          (let ((proc (expval->proc (value-of rator env)))
                )
            (apply-procedure proc parms exps)))

        )))

(define apply-procedure
  (lambda (proc1 parms exps)
    (cases proc proc1
      (procedure (ids defs body env)
                 (begin
                   ; 4 helper functions
                   (define (sublist? xs ys)
                     (or (null? xs)                 ; the empty list is a sublist of ys
                         (and                       ; for a non-empty list 
                          (member   (car xs) ys)    ;   the first element must be in ys
                          (sublist? (cdr xs) ys)))))
                 (define def-vals
                   (lambda (ids defs env orgEnv)
                     (if (null? ids)
                         env
                         (let ((newEnv (extend-env (car ids) (value-of (car defs) orgEnv) env)))
                           (def-vals (cdr ids) (cdr defs) newEnv orgEnv))
      
                         )))
                 (define parms-vals
                   (lambda (parms exps env orgEnv)
                     (if (null? exps)
                         env
                         (parms-vals (cdr parms) (cdr exps) (extend-env (car parms)  (value-of (car exps) env) env) orgEnv)) 
                     ))
                 (define idsInParms
                   (lambda (parms ids)
                     (if (null? parms)
                         #t
                         (if (member (car parms) ids)
                             (idsInParms (cdr parms) ids)
                             (eopl:error "incorrect variable(s) in the call.")))
              
                     ))
                 (idsInParms parms ids) ; check for error
            
                 (let ((newEnv (def-vals ids defs env env)))
                   (let ((newNEnv (parms-vals parms exps newEnv env)))
                     (value-of body newNEnv))        
                   )))))

  )
