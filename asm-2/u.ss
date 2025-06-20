(library (asm-2 u)
  (export
    u2? u2
    u3? u3
    u8? u8
    u16? u16)
  (import (micascheme))

  (define (u2? $obj)
    (and (integer? $obj) (>= $obj #b00) (<= $obj #b11)))

  (define (u3? $obj)
    (and (integer? $obj) (>= $obj #b000) (<= $obj #b111)))

  (define (u8? $obj)
    (and (integer? $obj) (>= $obj #x00) (<= $obj #xff)))

  (define (u16? $obj)
    (and (integer? $obj) (>= $obj #x0000) (<= $obj #xffff)))

  (define-rule-syntax (u2 expr)
    (switch expr
      ((u2? $u2) $u2)
      ((else $other)
        (syntax-error #'expr
          (format "expected u2, got ~s, in" $other)))))

  (define-rule-syntax (u3 expr)
    (switch expr
      ((u3? $u3) $u3)
      ((else $other)
        (syntax-error #'expr
          (format "expected u3, got ~s, in" $other)))))

  (define-rule-syntax (u8 expr)
    (switch expr
      ((u8? $u8) $u8)
      ((else $other)
        (syntax-error #'expr
          (format "expected u8, got ~s, in" $other)))))

  (define-rule-syntax (u16 expr)
    (switch expr
      ((u16? $u16) $u16)
      ((else $other)
        (syntax-error #'expr
          (format "expected u16, got ~s, in" $other)))))
)
