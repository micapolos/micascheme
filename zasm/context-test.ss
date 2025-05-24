(import (micascheme) (zasm context))

(check
  (context=?
    (make-context 2 (bytevector 1 2 3 4))
    (make-context 2 (bytevector 1 2 3 4))))

(check
  (context=?
    (lets
      ($context (make-context 1 (bytevector 0 0 0 0)))
      (run (context-db! $context 123))
      $context)
    (make-context 2 (bytevector 0 123 0 0))))

(check
  (context=?
    (lets
      ($context (make-context 1 (bytevector 0 0 0 0)))
      (run (context-dw! $context #x1234))
      $context)
    (make-context 3 (bytevector 0 #x34 #x12 0))))
