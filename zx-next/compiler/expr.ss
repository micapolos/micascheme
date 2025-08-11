(library (zx-next compiler expr)
  (export
    u8 u16 u24 u32
    ld-expr
    u8-inc u8-dec
    u8-add-n u8-sub-n u8-and-n u8-or-n u8-xor-n
    u8-add u8-sub u8-and u8-or u8-xor u8-mul
    u8-peek-nn u8-peek-local u8-peek
    u8-zero? u8=? u8>
    u16-inc u16-dec u16-add u16-sub
    u16-peek-nn)
  (import (zx-next core))

  (define-keywords
    u8 u16 u24 u32
    u8-inc u8-dec u8-add u8-sub u8-and u8-or u8-xor u8-mul
    u8-add-n u8-sub-n u8-and-n u8-or-n u8-xor-n
    u8-peek-nn u8-peek-local u8-peek
    u8-zero? u8=? u8>
    u16-inc u16-dec u16-add u16-sub
    u16-peek-nn)

  (define-ops
    (keywords
      a de hl
      u8 u16 u24 u32
      u8-inc u8-dec u8-add u8-sub u8-and u8-or u8-xor u8-mul
      u8-add-n u8-sub-n u8-and-n u8-or-n u8-xor-n
      u8-peek-nn u8-peek-local u8-peek
      u8-zero? u8=? u8>
      u16-inc u16-dec u16-add u16-sub
      u16-peek-nn)

    ; 8-bit load
    ((ld-expr r (u8 n))
      (ld r n))

    ((ld-expr a (u8-peek-nn nn))
      (ld a (nn)))

    ((ld-expr r (u8-peek-nn nn))
      (ld a (nn))
      (ld r a))

    ((ld-expr r (u8-peek lhs))
      (ld-expr hl lhs)
      (ld r (hl)))

    ((ld-expr r (u8-peek-local offset))
      (ld r (+ ix offset)))

    ; 8-bit increment/decrement
    ((ld-u8-op1 r op lhs)
      (ld-expr r lhs)
      (op r))

    ((ld-expr r (u8-inc lhs))
      (ld-u8-op1 r inc lhs))

    ((ld-expr r (u8-dec lhs))
      (ld-u8-op1 r dec lhs))

    ; 8-bit math with constant
    ((ld-u8-op2-n a op lhs n)
      (ld-expr a lhs)
      (op n))

    ((ld-u8-op2-n r op lhs n)
      (ld-u8-op2-n a op lhs n)
      (ld r a))

    ((ld-expr r (u8-add-n lhs n))
      (ld-u8-op2-n r add lhs n))
    ((ld-expr r (u8-sub-n lhs n))
      (ld-u8-op2-n r sub lhs n))
    ((ld-expr r (u8-and-n lhs n))
      (ld-u8-op2-n r and lhs n))
    ((ld-expr r (u8-or-n lhs n))
      (ld-u8-op2-n r or lhs n))
    ((ld-expr r (u8-xor-n lhs n))
      (ld-u8-op2-n r xor lhs n))

    ; 8-bit math
    ((ld-u8-op2 a op lhs rhs)
      (ld-expr l rhs)
      (push hl)
      (ld-expr a lhs)
      (pop hl)
      (op l))

    ((ld-u8-op2 r op lhs rhs)
      (ld-u8-op2 a op lhs rhs)
      (ld r a))

    ((ld-expr r (u8-add lhs rhs))
      (ld-u8-op2 r add lhs rhs))
    ((ld-expr r (u8-sub lhs rhs))
      (ld-u8-op2 r sub lhs rhs))
    ((ld-expr r (u8-and lhs rhs))
      (ld-u8-op2 r and lhs rhs))
    ((ld-expr r (u8-or lhs rhs))
      (ld-u8-op2 r or lhs rhs))
    ((ld-expr r (u8-xor lhs rhs))
      (ld-u8-op2 r xor lhs rhs))

    ; 8-bit mul
    ((ld-expr de (u8-mul lhs rhs))
      (ld-expr e rhs)
      (push de)
      (ld-expr a lhs)
      (pop de)
      (ld d a)
      (mul d e))

    ((ld-expr rr (u8-mul lhs rhs))
      (ld-expr de (u8-mul lhs rhs))
      (ld rr de))

    ; 16-bit
    ((ld-expr rr (u16 nn))
      (ld rr nn))

    ((ld-u16-op1 rr op lhs)
      (ld-expr rr lhs)
      (op rr))

    ((ld-expr rr (u16-inc lhs))
      (ld-u16-op1 rr inc lhs))

    ((ld-expr rr (u16-dec lhs))
      (ld-u16-op1 rr dec lhs))

    ((ld-expr hl (u16-add lhs rhs))
      (ld-expr rr rhs)
      (push hl)
      (ld-expr rr lhs)
      (pop de)
      (add hl de))

    ((ld-expr rr (u16-add lhs rhs))
      (ld-expr hl (u16-add lhs rhs))
      (ld rr hl))

    ((ld-expr hl (u16-sub lhs rhs))
      (ld-expr rr rhs)
      (push hl)
      (ld-expr rr lhs)
      (pop de)
      (rcf)
      (sbc hl de))

    ((ld-expr rr (u16-sub lhs rhs))
      (ld-expr hl (u16-sub lhs rhs))
      (ld rr hl))

    ((ld-expr hl (u16-peek-nn nn))
      (ld hl (nn)))

    ((ld-expr rr (u16-peek-nn nn))
      (ld hl (nn))
      (ld rr hl))

    ; Conditionals
    ((ld-expr r (if (u8-zero? lhs) then-body else-body))
      (ld-expr a lhs)
      (or a)
      (if z (ld-expr r then-body) (ld-expr r else-body)))

    ((ld-expr r (if (u8=? lhs rhs) then-body else-body))
      (ld-expr l lhs)
      (push hl)
      (ld-expr a rhs)
      (pop hl)
      (xor l)
      (if z (ld-expr r then-body) (ld-expr r else-body)))

    ((ld-expr r (if (u8> lhs rhs) then-body else-body))
      (ld-expr l lhs)
      (push hl)
      (ld-expr a rhs)
      (pop hl)
      (cp l)
      (if c (ld-expr r then-body) (ld-expr r else-body)))
  )
)
