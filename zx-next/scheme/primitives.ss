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
    local-value

    push-n
    push-nn

    inc-r
    dec-r

    print
    println
    write-stack

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
    (zx-next scheme value))

  ; Calling convention:
  ;  E - value stack offset, must be preserved
  ;  arguments - on the stack
  ;  IY - exit handler
  ;  HL - may contain return address, must be preserved

  (define-fragments
    (error-string (dz "Internal error"))
    (stack-string (dz "stack"))
    (unknown-string (dz "unknown")))

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
      (ld b word-tag))

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

    ((value->tag)
      (input (bcc value))
      (output (a tag))
      (ld a d)
      (and #b11100000))

    ((value->hl)
      (input (bc word))
      (output (bcd value))
      (ld h c)
      (ld l d))

    ((value->mmu/hl)
      (input (bcd value))
      (output (mmu paged-in) (hl address))
      (ld a d)   ; bank in D
      (mmu 7 a)
      (ld a b)   ; tag/addr in BC
      (or #b11100000)
      (ld h a)
      (ld l c))

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

  (define-values
    (normal-color   7)
    (symbol-color   3)
    (boolean-color  5)
    (char-color     5)
    (string-color   2)
    (paren-color    7)
    (number-color   6)
    (hash-color     5))

  (define-fragment write-open
    (write-ink paren-color)
    (ld a #\()
    (jp write-char))

  (define-fragment write-close
    (write-ink paren-color)
    (ld a #\))
    (call write-char)
    (write-ink normal-color)
    (ret))

  (define-fragment write-quotes
    (write-ink string-color)
    (ld a #\")
    (jp write-char))

  (define-fragment write-hash
    (write-ink hash-color)
    (ld a #\#)
    (jp write-char))

  (define-fragment write-space
    (ld a #\space)
    (jp write-char))

  (define-fragment write-true
    (call write-hash)
    (write-ink boolean-color)
    (ld a #\t)
    (jp write-char))

  (define-fragment write-false
    (call write-hash)
    (write-ink boolean-color)
    (ld a #\f)
    (jp write-char))

  (define-fragment write-null
    (call write-open)
    (jp write-close))

  (define-fragment write-hex-prefix
    (call write-hash)
    (ld a #\x)
    (call write-char)
    (write-ink number-color)
    (ret))

  (define-fragment write-byte-literal
    (input (a byte))
    (preserve (af) (call write-hex-prefix))
    (jp write-byte))

  (define-fragment write-char-literal
    (input (a char))
    (preserve (af)
      (call write-hash)
      (ld a #\\)
      (call write-char)
      (write-ink char-color))
    (jp write-char))

  (define-fragment write-word-literal
    (input (hl word))
    (preserve (hl) (call write-hex-prefix))
    (jp write-word))

  (define-fragment write-symbol
    (input (hl addr))
    (preserve (hl) (write-ink symbol-color))
    (call write-string)
    (ret))

  (define-fragment write-string-literal
    (input (hl addr))
    (preserve (hl) (call write-quotes))
    (call write-string)
    (jp write-quotes))

  (define-fragment write-symbolic
    (input (bcd value) (hl symbol-addr))
    (preserve (bc de)
      (preserve (hl) (call write-open))
      (call write-symbol))
    (call write-value)
    (jp write-close))

  (define-fragment write-unknown
    (call write-hash)
    (ld a #\<)
    (call write-char)
    (ld hl unknown-string)
    (call write-symbol)
    (ld a #\>)
    (jp write-char))

  (define-fragment write-value
    (input (bcd value))
    (ld a b)
    (and #b11100000)

    (cp byte-tag)
    (when z
      (value->a)
      (jp write-byte-literal))

    (cp word-tag)
    (when z
      (value->hl)
      (jp write-word-literal))

    (cp char-tag)
    (when z
      (value->a)
      (jp write-char-literal))

    (cp constant-tag)
    (when z
      (ld a b)

      (cp null-tag)
      (when z (jp write-null))

      (cp false-tag)
      (when z (jp write-false))

      (cp true-tag)
      (when z (jp write-true))

      (jp write-unknown))

    (cp symbol-tag)
    (when z
      (value->mmu/hl)
      (jp write-symbol))

    (cp string-tag)
    (when z
      (value->mmu/hl)
      (jp write-string-literal))

    (jp write-unknown))

  (define-fragment write-stack
    (preserve (de)
      (preserve (de)
        (call write-open)
        (ld hl stack-string)
        (call write-symbol))

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
          (call write-close)
          (call write-newline)
          (pop de)  ; compensate for (preserve (de)) - implement break from loop!!!
          (ret))

        ; advance to the next entry
        (add hl a)
        (preserve (hl)
          (preserve (bc de) (call write-space))
          (call write-value))))

    (ret))
)
