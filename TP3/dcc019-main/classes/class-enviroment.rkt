#lang racket

;Require n funcionou e não sei bem por que?
;(require (file "C:\\Users\\filip\\Documents\\Racket\\dcc019-main\\classes\\method.rkt"))
(struct ClassDescriptor (class-id super-class-id var-ids methods))

;Definindo Classes
(struct Prog (decls exp))
(struct Decl (class-id super-class-id var-ids methods))
(struct Method (method-id var-ids exp))
(struct Exp (exp-type exp-content))

;Construção de ambiente
(define (build-class-environment classes)
  (let ((class-env (make-hash)))
    (for-each
     (lambda (class-decl)
       (hash-set! class-env (ClassDescriptor-class-id class-decl) class-decl))
     classes)
    class-env))

;Exemplo para teste:
(define example-classes
  (list (ClassDescriptor 'MyClass 'SuperClass '(var1 var2)
                         (list (Method 'method1 '(arg1 arg2)
                                       (Exp 'INT 42))
                               (Method 'method2 '(arg1 arg2)
                                       (Exp 'VAR 'var1))))
        (ClassDescriptor 'SuperClass 'BaseClass '(var3 var4)
                         (list (Method 'method3 '(arg3 arg4)
                                       (Exp 'INT 84))))))

(define class-environment (build-class-environment example-classes))

(displayln class-environment)