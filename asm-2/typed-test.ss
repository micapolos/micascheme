(import (micascheme) (asm-2 typed))

(define-rule-syntax (check-typed in out)
  (check-datum=? (syntax->typed (empty-scope) #'in) 'out))

(check-typed type (typed type type))

(check-typed void (typed type void))
(check-typed boolean (typed type boolean))
(check-typed integer (typed type integer))
(check-typed char (typed type char))
(check-typed string (typed type string))

(check-typed
  (procedure (integer string) boolean)
  (typed type (procedure (integer string) boolean)))

(check-typed (void) (typed void (void)))
(check-typed #f (typed boolean #f))
(check-typed 123 (typed integer 123))
(check-typed #\a (typed char #\a))
(check-typed "foo" (typed string "foo"))

(check-typed + (typed (procedure (integer integer) integer) +))
(check-typed string-append (typed (procedure (string string) string) string-append))

(check-typed
  (lambda () "foo")
  (typed
    (procedure () string)
    (lambda ()"foo")))

(check-typed
  (lambda ((integer i) (string s)) i)
  (typed
    (procedure (integer string) integer)
    (lambda (i s) i)))

(check-typed
  (lambda ((integer i) (string s)) s)
  (typed
    (procedure (integer string) string)
    (lambda (i s) s)))

(check-typed
  ((lambda ((integer i)) i) 123)
  (typed integer ((lambda (i) i) 123)))
