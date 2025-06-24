(import (micascheme) (typico lookup) (typico core lookup) (typico expand) (typico typed))

(define-rule-syntax (check-typed in out)
  (check
    (equal?
      (typed->datum (expand-typed (core-lookup) #'in))
      'out)))

(check-typed
  (let () 123)
  (typed integer (let () 123)))

(check-typed
  (let ((x 10)) x)
  (typed integer (let ((x 10)) x)))

(check-typed
  (let ((x 10) (y 20)) (+ x y))
  (typed integer (let ((x 10) (y 20)) (($primitive 3 +) x y))))
