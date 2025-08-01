(library (zx-next scheme primitives)
  (export
    byte-value
    char-value
    word-value

    a->char-value

    push-value
    pop-value
    dup-value

    push-n
    push-nn

    add-r-n
    sub-r-n
    and-r-n
    or-r-n
    xor-r-n

    and-r-r
    or-r-r
    xor-r-r

    inc-r
    dec-r

    println
    println-stack

    byte-add
    byte-sub)
  (import (zx-next core) (zx-next write))

  (define-fragments
    (hello-world-string (dz "Hello, world!"))
    (stack-string (dz "stack")))

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

    ((byte-value n)
      (output (bcd value))
      (ld d n)
      (ld bc #b0000000000000000))

    ((word-value nn)
      (output (bcd value))
      (ld d (fxand nn #xff))
      (ld c (fxand (fxsrl nn 8) #xff))
      (ld b #b00100000))

    ((char-value n)
      (input (a byte))
      (output (bcd value))
      (ld d n)
      (ld bc #b0100000000000000))

    ((a->char-value)
      (input (a byte))
      (output (bcd value))
      (ld d a)
      (ld bc #b0100000000000000))

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

    ((dup-value)
      (pop bc)
      (pop de)
      (push de)
      (push bc)
      (push de)
      (push bc))

    ((dup-value offset)
      (preserve (hl)
        (ld hl (+ 2 (* 4 offset)))
        (add hl sp)
        (ld e (hl))
        (inc hl)
        (ld d (hl))
        (inc hl)
        (ld c (hl))
        (inc hl)
        (ld b (hl)))
      (push bc)
      (push de))

    ((alu-r-r op)
      (pop-a)
      (pop-d)
      (op d)
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

    ((byte-add)  (alu-r-r add))
    ((byte-sub)  (alu-r-r sub))
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

  (define-fragment println
    (pop hl)
    (pop-value)
    (preserve (hl)
      (call write-value)
      (call write-newline))
    (jp (hl)))

  (define-fragment write-value
    (ld a b)
    (and #b11100000)

    (cp #b00000000)
    (when z
      (ld a d)
      (preserve (af)
        (ld a #\#)
        (call write-char)
        (ld a #\x)
        (call write-char))
      (jp write-byte))

    (cp #b00100000)
    (when z
      (ld h c)
      (ld l d)
      (preserve (hl)
        (ld a #\#)
        (call write-char)
        (ld a #\x)
        (call write-char))
      (jp write-word))

    (cp #b01000000)
    (when z
      (ld a d)
      (preserve (af)
        (ld a #\#)
        (call write-char)
        (ld a #\\)
        (call write-char))
      (jp write-char))

    (ret))

  (define-fragment println-stack
    (ld a #\()
    (call write-char)
    (ld hl stack-string)
    (call write-string)

    (ld hl 2)
    (add hl sp)

    (loop
      ; Load value into bcde
      (ld e (hl))
      (inc hl)
      (ld d (hl))
      (inc hl)
      (ld c (hl))
      (inc hl)
      (ld b (hl))
      (inc hl)
      (ld a e)

      ; return if end of stack
      (cp #xff)
      (when z
        (ld a #\))
        (call write-char)
        (call write-newline)
        (ret))

      ; advance to the next entry
      (add hl a)
      (preserve (hl)
        (preserve (bc de)
          (ld a #\space)
          (call write-char))
        (call write-value)))

    (ret))
)
