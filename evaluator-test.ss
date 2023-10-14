(import (micascheme) (evaluator))

; construction

(lets 
  ($evaluator (empty-evaluator (environment `(micascheme))))
  ($evaluator (evaluator-push $evaluator (cons `a "Hello, ")))
  ($evaluator (evaluator-push $evaluator (cons `b "world!")))
  (check 
    (equal? 
      (evaluator-bindings $evaluator) 
      (stack 
        (cons `a "Hello, ") 
        (cons `b "world!")))))

; evaluate empty

(check 
  (equal? 
    (evaluate 
      (evaluator (environment `(micascheme)) (stack))
      `(string-append "Hello, " "world!"))
    "Hello, world!"))

; evaluate bindings 

(check
  (equal? 
    (evaluate 
      (evaluator
        (environment `(micascheme))
        (stack
          (cons `a "Hello, ") 
          (cons `b "world!")))
      `(string-append a b))
    "Hello, world!"))

; evaluate redefined bindings

(check
  (equal? 
    (evaluate 
      (evaluator
        (environment `(micascheme))
        (stack
          (cons `a "Hello, ") 
          (cons `b "world!")
          (cons `a "Goodbye, ")))
      `(string-append a b))
    "Goodbye, world!"))

; evaluate redefined environment

(check
  (equal? 
    (evaluate 
      (evaluator
        (environment `(micascheme))
        (stack 
          (cons `+ (lambda (a b) (string-append a b)))))
      `(+ "Hello, " "world!"))
    "Hello, world!"))
