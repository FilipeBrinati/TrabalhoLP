#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (value-of e Δ)]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (apply-env Δ v)]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (display "send expression unimplemented")]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) (display "new expression unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (store-assign! Δ x (value-of e Δ))]
    [(ast:print e) (display (value-of e Δ))]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (apply-stmts stmts Δ)]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ)
                              (result-of s1 Δ)
                              (result-of s2 Δ))]
    [(ast:while e s) (while-loop e s Δ)]
    [(ast:local-decl (ast:var x) s) (local-var-declaration x s Δ)]
    [(ast:send e (ast:var mth) args) (display "command send unimplemented")]
    [(ast:super (ast:var c) args) (display "command super unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; apply-stmts :: [Stmt] -> Env -> State -> State
(define (apply-stmts stmts Δ)
  (if (null? stmts)
      Δ
      (apply-stmts (cdr stmts) (result-of (car stmts) Δ))))

; while-loop :: Exp -> Stmt -> Env -> State -> State
(define (while-loop exp stmt Δ)
  (if (value-of exp Δ)
      (while-loop exp stmt (result-of stmt Δ))
      Δ))

; local-var-declaration :: Var -> Stmt -> Env -> State -> State
(define (local-var-declaration var stmt Δ)
  (let ([value (value-of (var-initializer var) Δ)])
    (result-of stmt (extend-env (var-name var) value Δ))))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       (let* ([class-env (empty-env)]
              [new-env (extend-class-env class-env decls empty-env)])
         (result-of stmt new-env)))]))

; extend-class-env :: Env -> [Decl] -> Env -> Env
(define (extend-class-env class-env decls env)
  (if (null? decls)
      env
      (extend-class-env class-env (cdr decls) (extend-env (decl-name (car decls)) (eval-decl (car decls) class-env) env))))

; eval-decl :: Decl -> Env -> Value
(define (eval-decl decl env)
  (match decl
    [(ast:decl name super fields methods)
     (let ([class-env (extend-env 'self name env)]
           [super-class (apply-env env super)])
       (let ([class (make-object (class-template super-class) fields methods class-env)])
         (extend-class-env class-env methods (extend-env name class env))))]))

; apply-env :: Env -> Var -> Value
(define (apply-env env var)
  (lookup-env env var))

; extend-env :: Var -> Value -> Env -> Env
(define (extend-env var value env)
  (extend-env var value env))

; lookup-env :: Env -> Var -> Value
(define (lookup-env env var)
  (apply-env env var))

; empty-env :: Env
(define empty-env (empty-env))

; empty-store :: State
(define empty-store (empty-store))

; store-assign! :: State -> Var -> Value -> State
(define store-assign! (store-assign!))

; class-template :: Class -> ClassTemplate
(define class-template (class-template))

; main :: [String] -> Void
(define (main args)
  (let ([prog
         (prog
          (list (decl "c1" "object" '())
                (decl "c2" "c1" '())
                (decl "c3" "c2" '()))
          (block
           (list
            (local-decl (var "o3") (block
                                    (list
                                     (assign "o3" (new "c3" '()))
                                     (print (send (var "o3") "m3"))))))))])
    (value-of-program prog)))

