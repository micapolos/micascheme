(library (zx-next compiler expr)
  (export
    ld-expr
    byte byte-inc byte-dec
    byte-add-n byte-sub-n byte-and-n byte-or-n byte-xor-n
    byte-add byte-sub byte-and byte-or byte-xor byte-mul
    byte-peek-nn byte-peek-local byte-peek
    word word-inc word-dec word-add word-sub
    word-peek-nn)
  (import (zx-next core))

  (define-keywords
    byte byte-inc byte-dec byte-add byte-sub byte-and byte-or byte-xor byte-mul
    byte-add-n byte-sub-n byte-and-n byte-or-n byte-xor-n
    byte-peek-nn byte-peek-local byte-peek
    word word-inc word-dec word-add word-sub
    word-peek-nn)

  (define-ops
    (keywords
      a de hl
      byte byte-inc byte-dec byte-add byte-sub byte-and byte-or byte-xor byte-mul
      byte-add-n byte-sub-n byte-and-n byte-or-n byte-xor-n
      byte-peek-nn byte-peek-local byte-peek
      word word-inc word-dec word-add word-sub
      word-peek-nn)

    ; 8-bit load
    ((ld-expr r (byte n))
      (ld r n))

    ((ld-expr a (byte-peek-nn nn))
      (ld a (nn)))

    ((ld-expr r (byte-peek-nn nn))
      (ld a (nn))
      (ld r a))

    ((ld-expr r (byte-peek lhs))
      (ld-expr hl lhs)
      (ld r (hl)))

    ((ld-expr r (byte-peek-local offset))
      (ld r (+ ix offset)))

    ; 8-bit increment/decrement
    ((ld-byte-op1 r op lhs)
      (ld-expr r lhs)
      (op r))

    ((ld-expr r (byte-inc lhs))
      (ld-byte-op1 r inc lhs))

    ((ld-expr r (byte-dec lhs))
      (ld-byte-op1 r dec lhs))

    ; 8-bit math with constant
    ((ld-byte-op2-n a op lhs n)
      (ld-expr a lhs)
      (op n))

    ((ld-byte-op2-n r op lhs n)
      (ld-byte-op2-n a op lhs n)
      (ld r a))

    ((ld-expr r (byte-add-n lhs n))
      (ld-byte-op2-n r add lhs n))
    ((ld-expr r (byte-sub-n lhs n))
      (ld-byte-op2-n r sub lhs n))
    ((ld-expr r (byte-and-n lhs n))
      (ld-byte-op2-n r and lhs n))
    ((ld-expr r (byte-or-n lhs n))
      (ld-byte-op2-n r or lhs n))
    ((ld-expr r (byte-xor-n lhs n))
      (ld-byte-op2-n r xor lhs n))

    ; 8-bit math
    ((ld-byte-op2 a op lhs rhs)
      (ld-expr l rhs)
      (push hl)
      (ld-expr a lhs)
      (pop hl)
      (op l))

    ((ld-byte-op2 r op lhs rhs)
      (ld-byte-op2 a op lhs rhs)
      (ld r a))

    ((ld-expr r (byte-add lhs rhs))
      (ld-byte-op2 r add lhs rhs))
    ((ld-expr r (byte-sub lhs rhs))
      (ld-byte-op2 r sub lhs rhs))
    ((ld-expr r (byte-and lhs rhs))
      (ld-byte-op2 r and lhs rhs))
    ((ld-expr r (byte-or lhs rhs))
      (ld-byte-op2 r or lhs rhs))
    ((ld-expr r (byte-xor lhs rhs))
      (ld-byte-op2 r xor lhs rhs))

    ; 8-bit mul
    ((ld-expr de (byte-mul lhs rhs))
      (ld-expr e rhs)
      (push de)
      (ld-expr a lhs)
      (pop de)
      (ld d a)
      (mul d e))

    ((ld-expr rr (byte-mul lhs rhs))
      (ld-expr de (byte-mul lhs rhs))
      (ld rr de))

    ; 16-bit
    ((ld-expr rr (word nn))
      (ld rr nn))

    ((ld-word-op1 rr op lhs)
      (ld-expr rr lhs)
      (op rr))

    ((ld-expr rr (word-inc lhs))
      (ld-word-op1 rr inc lhs))

    ((ld-expr rr (word-dec lhs))
      (ld-word-op1 rr dec lhs))

    ((ld-expr hl (word-add lhs rhs))
      (ld-expr rr rhs)
      (push hl)
      (ld-expr rr lhs)
      (pop de)
      (add hl de))

    ((ld-expr rr (word-add lhs rhs))
      (ld-expr hl (word-add lhs rhs))
      (ld rr hl))

    ((ld-expr hl (word-sub lhs rhs))
      (ld-expr rr rhs)
      (push hl)
      (ld-expr rr lhs)
      (pop de)
      (rcf)
      (sbc hl de))

    ((ld-expr rr (word-sub lhs rhs))
      (ld-expr hl (word-sub lhs rhs))
      (ld rr hl))

    ((ld-expr hl (word-peek-nn nn))
      (ld hl (nn)))

    ((ld-expr rr (word-peek-nn nn))
      (ld hl (nn))
      (ld rr hl)))
)
