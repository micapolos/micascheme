(import (micalang base) (micalang term) (micalang term-compile))

(define (default-native-compile $default $native)
  `(compiled ,$native))

(define-rule-syntax (check-compiles in out)
  (check (equal? (default-term-compile default-native-compile in) 'out)))

(define-rule-syntax (check-throws in)
  (check (raises (default-term-compile default-native-compile in))))

(check-throws type)

(check-compiles
  (variable 'zero?)
  zero?)

(check-compiles
  (native '(dupa jasiu))
  (compiled (dupa jasiu)))

(check-compiles
  (application (variable 'zero?) (variable 'x))
  (zero? x))

(check-compiles
  (abstraction 'x (native 'int) (lambda (x) (application x x)))
  (lambda (x) (x x)))

(check-compiles
  (abstraction #f (native 'int) (lambda (_) (application (variable 'a) (variable 'b))))
  (lambda (_) (a b)))

(check-throws (pi 'x (native 'int) (lambda (x) (pi x x))))
