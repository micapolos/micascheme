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

(lets
  ($evaluator
    (fluent
      '(scheme)
      (environment)
      (empty-evaluator)
      (evaluator+ 'a "foo")
      (evaluator+ 'b "bar")))
  (run
    (check (evaluator-bound? $evaluator 'b))
    (check (evaluator-bound? $evaluator 'a))
    (check (evaluator-bound? $evaluator 'string))
    (check (false? (evaluator-bound? $evaluator 'foo)))

    (check (equal? (evaluator-ref $evaluator 'a) "foo"))
    (check (equal? (evaluator-ref $evaluator 'b) "bar"))
    (check (equal? (evaluator-ref $evaluator 'string) string))
    (check (raises (evaluator-ref $evaluator 'foo)))))
