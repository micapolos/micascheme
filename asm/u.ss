(library (asm u)
  (export
    u2? u2
    u3? u3
    u8? u8
    u16? u16
    s8? s8)
  (import (micascheme))

  (define (u2? $obj)
    (and (integer? $obj) (>= $obj #b00) (<= $obj #b11)))

  (define (u3? $obj)
    (and (integer? $obj) (>= $obj #b000) (<= $obj #b111)))

  (define (u8? $obj)
    (and (integer? $obj) (>= $obj #x00) (<= $obj #xff)))

  (define (u16? $obj)
    (and (integer? $obj) (>= $obj #x0000) (<= $obj #xffff)))

  (define (s8? $obj)
    (and (integer? $obj) (>= $obj -128) (<= $obj 127)))

  (define-rules-syntax
    ((u2 obj)
      (u2 obj #'obj))
    ((u2 obj stx)
      (switch obj
        ((u2? $u2) $u2)
        ((else $other)
          (syntax-error #'stx
            (format "invalid value ~s, expected u2, in" $other))))))

  (define-rules-syntax
    ((u3 obj)
      (u3 obj #'obj))
    ((u3 obj stx)
      (switch obj
        ((u3? $u3) $u3)
        ((else $other)
          (syntax-error #'stx
            (format "invalid value ~s, expected u3, in" $other))))))

  (define-rules-syntax
    ((u8 obj)
      (u8 obj #'obj))
    ((u8 obj stx)
      (switch obj
        ((u8? $u8) $u8)
        ((else $other)
          (syntax-error #'stx
            (format "invalid value ~s, expected u8, in" $other))))))

  (define-rules-syntax
    ((u16 obj)
      (u16 obj #'obj))
    ((u16 obj stx)
      (switch obj
        ((u16? $u16) $u16)
        ((else $other)
          (syntax-error #'stx
            (format "invalid value ~s, expected u16, in" $other))))))

  (define-rules-syntax
    ((s8 obj)
      (s8 obj #'obj))
    ((s8 obj stx)
      (switch obj
        ((s8? $s8) $s8)
        ((else $other)
          (syntax-error #'stx
            (format "invalid value ~s, expected s8, in" $other))))))
)
