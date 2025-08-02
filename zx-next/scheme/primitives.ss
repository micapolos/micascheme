(library (zx-next scheme primitives)
  (export
    byte-value
    char-value
    word-value

    a->char-value

    push-af
    push-bc

    pop-af
    pop-bc

    push-null
    push-true
    push-false
    push-byte
    push-char
    push-word
    push-symbol
    push-string

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

    inc-r
    dec-r

    println
    println-stack

    byte-add
    byte-sub
    byte-and
    byte-or
    byte-xor
    byte-mul

    run-scheme
    throw)
  (import (zx-next core) (zx-next write) (zx-next panic))

  ; Calling convention:
  ;  E - value stack offset, must be preserved
  ;  arguments - on the stack
  ;  IY - exit handler
  ;  HL - may contain return address, must be preserved

  (define-fragments
    (error-string (dz "Internal error")))

  (define-values
    (value-header 0)
    (u8-tag  #b00000000)
    (u16-tag #b00100000))

  (define-ops
    ((reset-offset)
      (ld e 0))

    ((run-scheme body ...)
      (with-panic (with-stack body ...))
      (when c
        (preserve (hl) (write "PANIC!!! "))
        (call write-string)))

    ((throw)
      (ld hl error-string)
      (panic))

    ((throw string-address)
      (ld hl string-address)
      (panic))

    ((with-stack body ...)
      (ld a #xff)
      (push af)
      (inc sp)

      (reset-offset)

      body ...

      (dec sp)
      (pop af))

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

    ((value->de)
      (input (bc word))
      (output (bcd value))
      (ld d c)
      (ld e d))

    ((value->hl)
      (input (bc word))
      (output (bcd value))
      (ld h c)
      (ld l d))

    ((push-value)
      (input (bcd value) (e offset))
      (push bc)
      (push de))

    ((push-af)
      (push af)
      (inc e)
      (inc e))

    ((push-bc)
      (push bc)
      (inc e)
      (inc e))

    ((pop-af)
      (dec e)
      (dec e)
      (pop af))

    ((pop-bc)
      (dec e)
      (dec e)
      (pop bc))

    ((push-null)
      (ld d 0)
      (ld bc #b0110000000000000)
      (push-value))

    ((push-false)
      (ld d 0)
      (ld bc #b0111000000000000)
      (push-value))

    ((push-true)
      (ld d 0)
      (ld bc #b0111100000000000)
      (push-value))

    ((push-byte n)
      (ld d n)
      (ld bc 0)
      (push-value))

    ((push-char n)
      (ld d n)
      (ld bc #b0100000000000000)
      (push-value))

    ((push-word nn)
      (ld d (fxand nn #xff))
      (ld bc (fxior (fxsll #b00100000 8) (fxand (fxsrl nn 8) #xff)))
      (push-value))

    ((push-symbol nn)
      (ld d (fxand nn #xff))
      (ld bc (fxior (fxsll #b10000000 8) (fxand (fxsrl nn 8) #xff)))
      (push-value))

    ((push-string nn)
      (ld d (fxand nn #xff))
      (ld bc (fxior (fxsll #b10100000 8) (fxand (fxsrl nn 8) #xff)))
      (push-value))

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
      (dup-value 0))

    ((dup-value 0)
      (pop de)
      (pop bc)
      (push bc)
      (push de)
      (reset-offset)
      (push bc)
      (push de))

    ; TODO: Make optimized version for (dup-value 1), where alternate register set will be used.

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
      (reset-offset)
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
    ((byte-and)  (alu-r-r and))
    ((byte-or)   (alu-r-r or))
    ((byte-xor)  (alu-r-r xor))

    ((inc-r)    (inc/dec-r inc))
    ((dec-r)    (inc/dec-r dec))

    ((byte-mul)
      (pop-a)
      (pop-d)    ; d = lhs
      (ld b e)   ; preserve offset
      (ld e a)   ; e = rhs
      (mul d e)
      (ld c d)   ; high byte in c
      (ld d e)   ; low byte in d
      (ld e b)   ; restore offset
      (ld b u16-tag)
      (push-value)))

  (define-fragment println
    (pop hl)
    (pop-value)
    (preserve (de hl)
      (call write-value)
      (call write-newline))
    (jp (hl)))

  (define-fragment write-value
    (ld a b)
    (and #b11100000)

    ; byte
    (cp #b00000000)
    (when z
      (value->a)
      (preserve (af) (write "#x"))
      (jp write-byte))

    ; word
    (cp #b00100000)
    (when z
      (value->hl)
      (preserve (hl) (write "#x"))
      (jp write-word))

    ; char
    (cp #b01000000)
    (when z
      (value->a)
      (preserve (af) (write "#\\"))
      (jp write-char))

    ; constant
    (cp #b01100000)
    (when z
      (ld a b)

      ; null
      (cp #b01100000)
      (when z (write "()"))

      ; false
      (cp #b01110000)
      (when z (write "#f"))

      ; true
      (cp #b01111000)
      (when z (write "#t")))

    ; symbol
    (cp #b10000000)
    (when z
      (value->hl)
      (jp write-string))

    ; string
    (cp #b10100000)
    (when z
      (value->hl)
      (preserve (hl) (write #\"))
      (call write-string)
      (write #\"))

    (ret))

  (define-fragment println-stack
    (preserve (de)
      (preserve (de) (write "(stack"))

      (ld hl 4)
      (add hl sp)
      (ld a e)
      (add hl a)

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
          (writeln #\))
          (pop de)  ; compensate for (preserve (de)) - implement break from loop!!!
          (ret))

        ; advance to the next entry
        (add hl a)
        (preserve (hl)
          (preserve (bc de) (write #\space))
          (call write-value))))

    (ret))
)
