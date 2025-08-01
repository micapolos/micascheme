(library (zx-next scheme primitives)
  (export
    push-n
    push-nn

    add-r-n
    sub-r-n
    and-r-n
    or-r-n
    xor-r-n

    add-r-r
    sub-r-r
    and-r-r
    or-r-r
    xor-r-r

    inc-r
    dec-r)
  (import (zx-next core))

  (define-values
    (value-header 0)
    (u8-tag  #b00000000)
    (u16-tag #b00100000))

  (define-ops
    ((d->value)
      (input (a byte))
      (output (bcd value))
      (ld bc 0))

    ((a->value)
      (input (a byte))
      (output (bcd value))
      (ld d a)
      (ld bc 0))

    ((value->d)
      (input (bcd value))
      (output (a byte)))

    ((value->a)
      (input (bcd value))
      (output (a byte))
      (ld a d))

    ((bc->value)
      (input (bc word))
      (output (bcd value))
      (ld d c)
      (ld c b)
      (ld b u16-tag))

    ((value->bc)
      (input (bc word))
      (output (bcd value))
      (ld b c)
      (ld c d))

    ((push-value)
      (input (bcd value) (e offset))
      (push bc)
      (push de))

    ((pop-value)
      (output (bcd value) (e offset))
      (pop de)
      (pop bc))

    ((push-n n)
      (input (e offset))
      (ld d n)
      (push-d))

    ((push-a)
      (input (a value) (e offset))
      (a->value)
      (push-value))

    ((push-d)
      (input (d value) (e offset))
      (d->value)
      (push-value))

    ((pop-a)
      (output (a value) (e offset))
      (pop-value)
      (value->a))

    ((pop-d)
      (output (d value) (e offset))
      (pop-value)
      (value->d))

    ((push-nn nn)
      (input (e offset))
      (ld bc nn)
      (bc->value)
      (push-value))

    ((push-bc)
      (input (bc value) (e offset))
      (bc->value)
      (push-value))

    ((pop-bc)
      (output (bc value) (e offset))
      (pop-value)
      (value->bc))

    ((alu-r-r op)
      (ex af)
      (pop-a)
      (ex af)
      (pop-a)
      (ex af)
      (ld b a)
      (ex af)
      (op b)
      (push-a))

    ((inc/dec-r op)
      (pop-d)
      (op d)
      (push-d))

    ((add-r-n)  (alu-r-n add))
    ((sub-r-n)  (alu-r-n sub))
    ((and-r-n)  (alu-r-n and))
    ((or-r-n)   (alu-r-n or))
    ((xor-r-n)  (alu-r-n xor))

    ((add-r-r)  (alu-r-r add))
    ((sub-r-r)  (alu-r-r sub))
    ((and-r-r)  (alu-r-r and))
    ((or-r-r)   (alu-r-r or))
    ((xor-r-r)  (alu-r-r xor))

    ((inc-r)    (inc/dec-r inc))
    ((dec-r)    (inc/dec-r dec))

    ((mul-r-r)
      (pop-value)
      (value->a)   ; a = rhs
      (pop-value)
      (value->d)   ; d = lhs
      (ld e a)
      (mul d e)
      (de->value)
      (push-value)))
)
