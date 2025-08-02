(library (zx-next scheme primitives)
  (export
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
    local-value

    push-n
    push-nn

    inc-r
    dec-r

    print
    println

    byte-add
    byte-sub
    byte-and
    byte-or
    byte-xor
    byte-mul

    run-scheme
    throw)
  (import
    (zx-next core)
    (zx-next write)
    (zx-next mmu)
    (zx-next panic)
    (zx-next scheme tag)
    (zx-next scheme value)
    (zx-next scheme write))

  ; Calling convention:
  ;  E - value stack offset, must be preserved
  ;  arguments - on the stack
  ;  IY - exit handler
  ;  HL - may contain return address, must be preserved

  (define-fragments
    (error-string (dz "Internal error")))

  (define-ops
    ((reset-offset)
      (ld e 0))

    ((inc-offset)
      (inc e)
      (inc e))

    ((dec-offset)
      (dec e)
      (dec e))

    ((run-scheme body ...)
      (with-panic (with-stack body ...))
      (when c
        (preserve (hl) (write "(error "))
        (call write-string)))

    ((throw)
      (pop-value)
      (panic))

    ((with-stack body ...)
      (ld a #xff)
      (push af)
      (inc sp)

      (reset-offset)

      body ...

      (dec sp)
      (pop af))

    ((push-value)
      (input (bcd value) (e offset))
      (push bc)
      (push de))

    ((push-af)
      (push af)
      (inc-offset))

    ((push-bc)
      (push bc)
      (inc-offset))

    ((pop-af)
      (dec-offset)
      (pop af))

    ((pop-bc)
      (dec-offset)
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

    ((push-symbol bank addr)
      (ld d bank)
      (ld bc (fxior (fxsll #b100 13) (fxand #x1fff addr)))
      (push-value))

    ((push-string bank addr)
      (ld d bank)
      (ld bc (fxior (fxsll #b101 13) (fxand #x1fff addr)))
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

    ((local-enter)
      (push ix)
      (inc-offset)
      (ld ix 0)
      (add ix sp))

    ((local-exit)
      (dec-offset)
      (pop ix))

    ((with-local body ...)
      (local-enter)
      body ...
      (local-exit))

    ((local-value offset)
      (ld b (+ ix 0))
      (ld c (+ ix 1))
      (ld d (+ ix 2))
      (reset-offset))

    ((alu-r-n op n)
      (pop-a)
      (op n)
      (push-a))

    ((alu-r-r op)
      (pop-a)
      (pop-d)
      (op d)
      (push-a))

    ((inc/dec-r op)
      (pop-d)
      (op d)
      (push-d))

    ((byte-add n)  (alu-r-n add))
    ((byte-sub n)  (alu-r-n sub))
    ((byte-and n)  (alu-r-n and))
    ((byte-or n)   (alu-r-n or))
    ((byte-xor n)  (alu-r-n xor))

    ((byte-add)  (alu-r-r add))
    ((byte-sub)  (alu-r-r sub))
    ((byte-and)  (alu-r-r and))
    ((byte-or)   (alu-r-r or))
    ((byte-xor)  (alu-r-r xor))

    ((inc-r)    (inc/dec-r inc))
    ((dec-r)    (inc/dec-r dec))

    ((byte-mul n)
      (pop-d)    ; d = lhs
      (ld b e)   ; preserve offset
      (ld e n)   ; e = rhs
      (mul d e)
      (ld c d)   ; high byte in c
      (ld d e)   ; low byte in d
      (ld e b)   ; restore offset
      (ld b word-tag)
      (push-value))

    ((byte-mul)
      (pop-a)
      (pop-d)    ; d = lhs
      (ld b e)   ; preserve offset
      (ld e a)   ; e = rhs
      (mul d e)
      (ld c d)   ; high byte in c
      (ld d e)   ; low byte in d
      (ld e b)   ; restore offset
      (ld b word-tag)
      (push-value)))

  (define-fragment print
    (pop hl)
    (pop-value)
    (preserve (de hl) (call write-value))
    (jp (hl)))

  (define-fragment println
    (pop hl)
    (pop-value)
    (preserve (de hl)
      (call write-value)
      (call write-newline))
    (jp (hl)))
)
