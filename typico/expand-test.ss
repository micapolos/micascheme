(import (micascheme) (typico expand))

(define (lookup $identifier)
  (syntax-case $identifier ($integer inc - macro)
    ($integer
      (lambda ($lookup $syntax)
        (syntax-case $syntax ($integer)
          ($integer (typed integer-type '$expanded-integer))
          (other (default-expand-typed $lookup #'other)))))
    (inc
      (lambda ($lookup $syntax)
        (syntax-case $syntax (inc)
          (inc
            (typed
              (function-type (list integer-type) integer-type)
              'expanded-inc))
          (other
            (default-expand-typed $lookup #'other)))))
    (-
      (lambda ($lookup $syntax)
        (syntax-case $syntax (-)
          (-
            (typed
              (function-type (list* integer-type integer-type) integer-type)
              'expanded-))
          (other
            (default-expand-typed $lookup #'other)))))
    (macro
      (lambda ($lookup $syntax)
        (syntax-case $syntax (macro)
          (macro (typed integer-type 'expanded-macro))
          (other (typed integer-type `(expanded ,(datum other)))))))
    (other
      (syntax-error #'other "unbound"))))

(define-rule-syntax (check-typed in out)
  (check
    (equal?
      (typed->datum (expand-typed lookup #'in))
      'out)))

(define-rule-syntax (check-type-error in)
  (check (raises (expand-typed lookup #'in))))

(check-typed #t (typed boolean #t))
(check-typed 123 (typed integer 123))

(check-typed $integer (typed integer $expanded-integer))
(check-type-error ($integer))

(check-typed inc (typed (function (integer) integer) expanded-inc))
(check-typed (inc $integer) (typed integer (expanded-inc $expanded-integer)))

(check-type-error (inc))
(check-type-error (inc $boolean))
(check-type-error (inc $integer $integer))

(check-typed - (typed (function (integer integer ...) integer) expanded-))
(check-type-error (-))
(check-typed (- $integer) (typed integer (expanded- $expanded-integer)))
(check-typed (- $integer $integer) (typed integer (expanded- $expanded-integer $expanded-integer)))
(check-type-error (- $boolean))
(check-type-error (- $boolean $integer))
(check-typed macro (typed integer expanded-macro))
(check-typed (macro 123) (typed integer (expanded (macro 123))))
