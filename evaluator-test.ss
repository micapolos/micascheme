(import (micascheme) (evaluator))

; empty bindings 

(check 
  (equal? 
    (evaluate 
      (evaluator (environment `(micascheme)) (list))
      `(string-append "Hello, " "world!"))
    "Hello, world!"))

; non-empty bindings 

(check
  (equal? 
    (evaluate 
      (evaluator
        (environment `(micascheme))
        (list
          (cons `a "Hello, ") 
          (cons `b "world!")))
      `(string-append a b))
    "Hello, world!"))

; redefine binding

(check
  (equal? 
    (evaluate 
      (evaluator
        (environment `(micascheme))
        (list
          (cons `a "Hello, ") 
          (cons `b "world!")
          (cons `a "Goodbye, ")))
      `(string-append a b))
    "Goodbye, world!"))

; redefine environment

(check
  (equal? 
    (evaluate 
      (evaluator
        (environment `(micascheme))
        (list 
          (cons `+ (lambda (a b) (string-append a b)))))
      `(+ "Hello, " "world!"))
    "Hello, world!"))
