#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (value-of e Δ)]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:decl class-name super-name field-names method-decls)
     (begin
       (initialize-class-decl class-name super-name field-names method-decls)
       'uninitialized)] ; Return 'uninitialized' for class declarations
    [(ast:var v)
     (let ([val (apply-env Δ v)])
       (if (eq? val 'no-bind)
           (raise-user-error "Variable not found: " v)
           (if (object? val)
               val
               (deref val))))] ; <- Handle "no bind"
    [(ast:let (var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
   [(ast:new (var c) args)
     (let ([class (lookup-class (value-of c Δ))])
       (if (void? class)
           (display "Class not found\n")
           (let ([obj (new-object (lookup-class-name class) (values-of-exps args Δ))])
             (set! Δ (extend-env (ast:var c) obj Δ))
             obj)))]
    [(ast:send e (var mth) args) (apply-method e mth (values-of-exps args Δ) Δ)]
    [(ast:super (var c) args) (apply-method (ast:self) c (values-of-exps args Δ) Δ)]
    [(ast:self) (apply-env Δ '%self)]
    [e (raise-user-error "unimplemented-construction-value-of: " e)]
    ))

(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (var x) e)
     (setref! (apply-env Δ x) (value-of e Δ))
     42]
    [(ast:print e) (display (value-of e Δ))
                   (newline)
                   '()]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (foldl (lambda (s v) (result-of s Δ)) '() stmts)]
    [(ast:if-stmt e s1 s2)
     (if (value-of e Δ)
         (result-of s1 Δ)
         (result-of s2 Δ))]
    [(ast:while e s)
     (if (value-of e Δ)
         (begin
           (result-of s Δ)
           (result-of (ast:while e s) Δ))
         '())]
    [(ast:local-decl (var x) s)
     (let ([loc (newref 'uninitialized)])
       (result-of s (extend-env x loc Δ)))]
    [(ast:send e (var mth) args)
     (let ([obj (value-of e Δ)])
       (apply-method obj mth (values-of-exps args Δ) Δ))]
    [(ast:super (var c) args)
     (let ([obj (apply-env Δ '%self)])
       (apply-method obj c (values-of-exps args Δ) Δ))]
    [(ast:decl _ _ _ _)
     '()] ; Skip class declarations
    [e (raise-user-error "unimplemented-construction-result-of: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  (initialize-class-env) ; Initialize the class environment
  (let ([init-env (extend-env '%self 'no-bind empty-env)]) ; Initialize the environment with '%self binding
    (match prog
      [(ast:prog decls stmt)
       (begin
         (for-each (lambda (decl) (value-of decl init-env)) decls)
         (result-of stmt init-env))])))


(define values-of-exps
  (lambda (exps env)
    (map (lambda (exp) (value-of exp env)) exps)))

(define (apply-env env var)
  (let ([val (env var)])
    (if (object? val)
        val
        (deref val))))


(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

(define (apply-proc proc val)
  (proc val))


(struct object (class-name fields) #:transparent)


(define new-object
  (lambda (class-name args)
    (object class-name (list->fields args))))

(define (list->fields args)
  (map newref args))

(define (apply-method obj mth args Δ)
  (let ([class (lookup-class (object-class-name obj))])
    (if (void? class)
        (raise-user-error "Class not found")
        (let ([method (find-method (ast:decl-methods class) mth)])
          (if (void? method)
              (raise-user-error "Method not found")
              (result-of (ast:method-body method)
                         (extend-env (ast:method-params method) args
                                     (extend-env '%self obj
                                                 (extend-env '%super
                                                             (ast:decl-super class)
                                                             Δ)))))))))

(define the-class-env '())

(define (initialize-class-env)
  (set! the-class-env
        (list (ast:decl "object" #f '() '()))))

(define (lookup-class name)
  (let ([class (find-name name the-class-env)])
    (if (void? class)
        (begin
          (display "Class not found\n")
          (ast:decl name #f '() '()))
        class)))


(define (lookup-class-name class)
  (match class
    [(ast:decl name _ _ _) name]
    [else (error "Invalid class declaration")]))

(define (find-name name lst)
  (cond
    [(null? lst) '()]
    [(and (ast:var? (car lst))
          (string=? (ast:var-name (car lst)) (ast:var-name name)))
     (car lst)]
    [else (find-name name (cdr lst))]))

(define (merge-method-envs super-m-env new-m-env)
  (append new-m-env super-m-env))

(define (initialize-class-decl class-name super-name field-names m-decls)
  (let* ([super-fields (if (eq? super-name #f)
                           '()
                           (ast:decl-fields (lookup-class super-name)))]
         [field-names (append-field-names super-fields field-names)])
    (add-to-class-env
     (ast:decl class-name super-name field-names
               (merge-method-envs
                (ast:decl-methods (lookup-class super-name))
                (method-decls->method-env
                 m-decls class-name))))))




(define (add-to-class-env class)
  (set! the-class-env
        (cons class the-class-env)))

(define (append-field-names super-fields new-fields)
  (cond
    [(null? super-fields) new-fields]
    [else
     (cons
      (if (memq (car super-fields) new-fields)
          (fresh-identifier (car super-fields))
          (car super-fields))
      (append-field-names (cdr super-fields) new-fields))]))

(define (fresh-identifier identifier)
  (let ([sn 0])
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"
        (number->string sn))))))

(define (find-method m-decls mth)
  (let ([method (find-name mth m-decls)])
    (if (void? method)
        (display "Method not found\n")
        method)))

(define (method-decls->method-env m-decls super-name)
  (map (lambda (m-decl)
         (match-define (ast:method method-name params body) m-decl)
         (ast:method method-name (append params (list '%self '%super)) body))
       m-decls))