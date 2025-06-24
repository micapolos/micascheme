(import (micascheme) (typico expand))

(define (lookup $identifier)
  (syntax-case $identifier ($integer inc - macro)
    ($integer
      (lambda ($lookup $syntax)
        (syntax-case $syntax ($integer)
          ($integer
            (typed integer-type '$resolved-integer))
          (other
            (default-expand-typed $lookup #'other)))))
    (inc
      (lambda ($lookup $syntax)
        (syntax-case $syntax (inc)
          (inc
            (typed
              (function-type (list integer-type) integer-type)
              'resolved-inc))
          (other
            (default-expand-typed $lookup #'other)))))
    (-
      (lambda ($lookup $syntax)
        (syntax-case $syntax (-)
          (-
            (typed
              (function-type (list* integer-type integer-type) integer-type)
              'resolved-))
          (other
            (default-expand-typed $lookup #'other)))))
    (macro
      (lambda ($lookup $syntax)
        (syntax-case $syntax (macro)
          (macro
            (typed integer-type '(expanded macro)))
          (other
            (typed integer-type `(expanded ,(datum other)))))))
    (other
      (syntax-error #'other "unbound"))))

(check
  (equal?
    (expand-typed lookup #'#t)
    (typed boolean-type #t)))

(check
  (equal?
    (expand-typed lookup #'123)
    (typed integer-type 123)))

(check
  (equal?
    (expand-typed lookup #'123)
    (typed integer-type 123)))

(check
  (equal?
    (expand-typed lookup #'$integer)
    (typed integer-type '$resolved-integer)))

(check
  (raises
    (expand-typed lookup #'($integer))))

(check
  (equal?
    (expand-typed lookup #'inc)
    (typed (function-type (list integer-type) integer-type) 'resolved-inc)))

(check
  (equal?
    (expand-typed lookup #'(inc $integer))
    (typed integer-type '(resolved-inc $resolved-integer))))

(check
  (raises
    (expand-typed lookup #'(inc))))

(check
  (raises
    (expand-typed lookup #'(inc $boolean))))

(check
  (raises
    (expand-typed lookup #'(inc $integer $integer))))

(check
  (equal?
    (expand-typed lookup #'-)
    (typed (function-type (list* integer-type integer-type) integer-type) 'resolved-)))

(check
  (raises
    (expand-typed lookup #'(-))))

(check
  (equal?
    (expand-typed lookup #'(- $integer))
    (typed integer-type '(resolved- $resolved-integer))))

(check
  (equal?
    (expand-typed lookup #'(- $integer $integer))
    (typed integer-type '(resolved- $resolved-integer $resolved-integer))))

(check
  (raises
    (expand-typed lookup #'(- $boolean))))

(check
  (raises
    (expand-typed lookup #'(- $boolean $integer))))

(check
  (equal?
    (expand-typed lookup #'macro)
    (typed integer-type '(expanded macro))))

(check
  (equal?
    (expand-typed lookup #'(macro 123))
    (typed integer-type '(expanded (macro 123)))))
