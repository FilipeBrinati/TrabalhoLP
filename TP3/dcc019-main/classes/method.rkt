#lang racket

(struct Prog (decls exp))
(struct Decl (class-id super-class-id var-ids methods))
(struct Method (method-id var-ids exp))
(struct Exp (exp-type exp-content))

(define example-program
  (Prog
   (list (Decl 'MyClass 'SuperClass '(var1 var2)
                (list (Method 'method1 '(arg1 arg2)
                              (Exp 'INT 42))
                      (Method 'method2 '(arg1 arg2)
                              (Exp 'VAR 'var1)))))
   (Exp 'VAR 'var1)))

(displayln example-program)