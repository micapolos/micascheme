(library (int)
  (export
    int-mask
    int
    int+
    int-
    int*
    int+1
    int-1)
  (import (scheme) (syntax) (lets))

  (define-case-syntax (int-mask width)
    (lets
      ($width (datum width))
      (cond
        ((not (integer? $width))
          (syntax-error #'width "non-integer width"))
        ((negative? $width)
          (syntax-error #'width "negative width"))
        ((> $width (fixnum-width))
          (syntax-error #'width (format "maximum ~a exceeded in width" (fixnum-width))))
        (else
          (literal->syntax (- (bitwise-arithmetic-shift-left 1 $width) 1))))))

  (define-rule-syntax (int width i)
    (fxand i (int-mask width)))

  (define-rule-syntax (int+ width a b)
    (int width (fx+/wraparound a b)))

  (define-rule-syntax (int- width a b)
    (int width (fx-/wraparound a b)))

  (define-rule-syntax (int* width a b)
    (int width (fx*/wraparound a b)))

  (define-rule-syntax (int+1 width a)
    (int+ width a 1))

  (define-rule-syntax (int-1 width a)
    (int- width a 1))
)
