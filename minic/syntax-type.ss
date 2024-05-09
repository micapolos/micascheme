(library (minic syntax-type)
  (export syntax->type)
  (import (micascheme) (minic runtime) (minic type))

  (define (syntax->bits $syntax)
    (lets
      ($datum (syntax->datum $syntax))
      (run
        (unless
          (and (integer? $datum) (>= $datum 0) (<= $datum (fixnum-width)))
          (syntax-error $syntax "bits outside of range")))
      $datum))

  (define (type-syntax->type $syntax)
    (syntax-case $syntax (int)
      ((int bits)
        (int-type (syntax->bits #'bits)))))

  (define (syntax->type $syntax)
    (syntax-case $syntax (int const add sub inc dec)
      ((int bits const value)
        (lets
          ($bits (syntax->bits #'bits))
          ($mask (fx-/wraparound (fxsll 1 $bits) 1))
          ($datum (datum value))
          ($fixnum
            (if (fixnum? $datum)
              $datum
              (syntax-error #'value "not fixnum")))
          ($valid? (fxzero? (fxand $fixnum (fxnot $mask))))
          (if $valid?
            (int-type $bits)
            (syntax-error $syntax "value outside of range"))))
      ((int bits inc rhs)
        (lets
          ($type (type-syntax->type #'(int bits)))
          (run (syntax-type-check #'rhs $type))
          $type))
      ((int bits dec rhs)
        (lets
          ($type (type-syntax->type #'(int bits)))
          (run (syntax-type-check #'rhs $type))
          $type))
      ((int bits add lhs rhs)
        (lets
          ($type (type-syntax->type #'(int bits)))
          (run (syntax-type-check #'lhs $type))
          (run (syntax-type-check #'rhs $type))
          $type))
      ((int bits sub lhs rhs)
        (lets
          ($type (type-syntax->type #'(int bits)))
          (run (syntax-type-check #'lhs $type))
          (run (syntax-type-check #'rhs $type))
          $type))))

  (define (syntax-type-check $syntax $expected-type)
    (lets
      ($type (syntax->type $syntax))
      (unless (equal? $type $expected-type)
        (syntax-error $syntax
          (format "expected type: ~a, actual type: ~a, syntax:"
            (type->datum $expected-type)
            (type->datum $type))))))
)
