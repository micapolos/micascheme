(import (scheme) (check) (syntax) (to-syntax) (lets) (procedure))

(check
  (equal?
    (syntax-inline (bytevector->syntax (bytevector 1 2 3)))
    (bytevector 1 2 3)))

(check
  (equal?
    (syntax-inline (bytevector->immutable-syntax (bytevector 1 2 3)))
    (bytevector 1 2 3)))

(check
  (equal?
    (syntax-inline
      (vector->syntax
        (vector (bytevector 1 2 3) (bytevector 4 5))
        bytevector->syntax))
    (vector (bytevector 1 2 3) (bytevector 4 5))))

(check
  (equal?
    (syntax-inline
      (vector->immutable-syntax
        (vector (bytevector 1 2 3) (bytevector 4 5))
        bytevector->syntax))
    (vector (bytevector 1 2 3) (bytevector 4 5))))

