(import (asm-3 base) (asm-3 expression) (asm-3 dependent) (asm lookable) (asm-2 relocable) (syntax lookup))

(check (expression? (dependent-with () #'foo)))

; pure-expression
(check-expression
  (pure-expression #'123)
  (dependent 123))

; identifier-expression
(check-expression
  (identifier-expression #'foo)
  (dependent (foo) foo))

; application-expression
(check-expression
  (application-expression
    (pure-expression #'+)
    (identifier-expression #'val-10)
    (identifier-expression #'val-20)
    (pure-expression #'1))
  (dependent (val-10 val-20)
    (+ val-10 val-20 1)))

; map-expression
(check-expression
  (map-expression
    (lambda ($expression) #`(u8-binary #,$expression))
    (identifier-expression #'foo))
  (dependent (foo) (u8-binary foo)))

; map-expressions
(check-expression
  (map-expressions list->syntax
    (list
      (pure-expression #'10)
      (identifier-expression #'foo)
      (identifier-expression #'bar)))
  (dependent (foo bar) (10 foo bar)))
