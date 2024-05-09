(library (minic runtime)
  (export
    i+1/wrap i-1/wrap
    i+/wrap i-/wrap)
  (import (micascheme))

  (define-syntax (i-mask $syntax)
    (syntax-case $syntax ()
      ((_ bits)
        (lets
          ($datum (datum bits))
          (if (and (integer? $datum) (>= $datum 0) (<= $datum (fixnum-width)))
            (datum->syntax #'i-mask (sub1 (fxsll 1 $datum)))
            (syntax-error $syntax (format "outside 0..~s range:" (fixnum-width))))))))

  (define-rule-syntax (i+/wrap bits lhs rhs)
    (($primitive 3 fxand)
      (i-mask bits)
      (($primitive 3 fx+/wraparound) lhs rhs)))

  (define-rule-syntax (i-/wrap bits lhs rhs)
    (($primitive 3 fxand)
      (i-mask bits)
      (($primitive 3 fx-/wraparound) lhs rhs)))

  (define-rule-syntax (i+1/wrap bits rhs)
    (i+/wrap bits rhs 1))

  (define-rule-syntax (i-1/wrap bits rhs)
    (i-/wrap bits rhs 1))
)
