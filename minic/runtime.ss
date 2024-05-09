(library (minic runtime)
  (export
    i-mask
    i
    i+1 i-1
    i+ i-
    int const clamp add sub inc dec)
  (import (micascheme) (syntaxes))

  (define-aux-keywords const clamp add sub inc dec)

  (define-rule-syntax (int-mask bits) (i-mask bits))

  (define-rules-syntaxes (literals const clamp add sub inc dec)
    ((int bits const value) value)
    ((int bits clamp rhs) (($primitive 3 fxand) (int-mask bits) rhs))
    ((int bits add lhs rhs) (int bits clamp (($primitive 3 fx+/wraparound) lhs rhs)))
    ((int bits sub lhs rhs) (int bits clamp (($primitive 3 fx-/wraparound) lhs rhs)))
    ((int bits inc rhs) (int bits add rhs 1))
    ((int bits dec rhs) (int bits sub rhs 1)))

  ; remove everything below
  (define-syntax (i-mask $syntax)
    (syntax-case $syntax ()
      ((_ bits)
        (lets
          ($datum (datum bits))
          (if (and (integer? $datum) (>= $datum 0) (<= $datum (fixnum-width)))
            (datum->syntax #'i-mask (fx-/wraparound (fxsll 1 $datum) 1))
            (syntax-error $syntax (format "outside 0..~s range:" (fixnum-width))))))))

  (define-rule-syntax (i bits value) value)

  (define-rule-syntax (i+ bits lhs rhs)
    (($primitive 3 fxand)
      (i-mask bits)
      (($primitive 3 fx+/wraparound) lhs rhs)))

  (define-rule-syntax (i- bits lhs rhs)
    (($primitive 3 fxand)
      (i-mask bits)
      (($primitive 3 fx-/wraparound) lhs rhs)))

  (define-rule-syntax (i+1 bits rhs)
    (i+ bits rhs 1))

  (define-rule-syntax (i-1 bits rhs)
    (i- bits rhs 1))
)
