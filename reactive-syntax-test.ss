(import (micascheme) (reactive-syntax))

(check
  (equal?
    (reactive-vector
      (syntax-reactive #`128)
      5)
    (vector 128 128 128 128 128)))

(check
  (equal?
    (reactive-vector (counter) 5)
    (vector 0 1 2 3 4)))
