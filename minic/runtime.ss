(library (minic runtime)
  (export int const extend clamp add sub inc dec)
  (import (micascheme) (syntaxes))

  (define-keywords const extend clamp add sub inc dec)

  (define-syntax (int-mask $syntax)
    (syntax-case $syntax ()
      ((_ bits)
        (lets
          ($datum (datum bits))
          (if (and (integer? $datum) (>= $datum 0) (<= $datum (fixnum-width)))
            (datum->syntax #'i-mask (fx-/wraparound (fxsll 1 $datum) 1))
            (syntax-error $syntax (format "outside 0..~s range:" (fixnum-width))))))))

  (define-rules-syntaxes (literals const extend clamp add sub inc dec)
    ((int bits const value) value)
    ((int bits extend rhs) rhs)
    ((int bits clamp rhs) (($primitive 3 fxand) (int-mask bits) rhs))
    ((int bits add lhs rhs) (int bits clamp (($primitive 3 fx+/wraparound) lhs rhs)))
    ((int bits sub lhs rhs) (int bits clamp (($primitive 3 fx-/wraparound) lhs rhs)))
    ((int bits inc rhs) (int bits add rhs 1))
    ((int bits dec rhs) (int bits sub rhs 1)))
)
