(library (u)
  (export
    u2? u2
    u3? u3
    u7? u7
    u8? u8
    u16? u16
    s8? s8)
  (import (micascheme))

  (define-case-syntax (define-integer sign bits)
    (lets
      ($signed? (syntax-case #'sign (u s) (u #f) (s #t)))
      ($bits (datum bits))
      ($size (bitwise-arithmetic-shift-left 1 $bits))
      ($min (if $signed? (- (bitwise-arithmetic-shift-right $size 1)) 0))
      ($max (+ $min $size))
      ($bits-id (literal->syntax (string->symbol (number->string $bits))))
      ($id (identifier-append #'sign #'sign $bits-id))
      ($id? (identifier-append #'sign $id #'?))
      #`(begin
        (define (#,$id? $obj)
          (and
            (integer? $obj)
            (>= $obj #,(literal->syntax $min))
            (< $obj #,(literal->syntax $max))))
        (define-rules-syntax
          ((#,$id obj)
            (#,$id obj obj))
          ((#,$id obj stx)
            (switch obj
              ((#,$id? $match) $match)
              ((else $mismatch)
                (syntax-error #'stx
                  (format "~s is not ~s, in" $mismatch '#,$id)))))))))

  (define-integer u 2)
  (define-integer u 3)
  (define-integer u 7)
  (define-integer u 8)
  (define-integer u 16)
  (define-integer s 8)
)
