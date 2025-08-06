(library (zx-next scheme prims)
  (export
    value
    byte-value
    word-value
    value-data
    pair-data
    box box-tc
    unbox unbox-tc
    car car-tc
    cdr cdr-tc
    cons cons-tc)
  (import
    (zx-next core)
    (zx-next scheme alloc)
    (zx-next banked-pointer)
    (zx-next scheme tag))

  ; All procedures use __sdcccall(1) calling convention.
  ; There are two types of procedures: primitive and non-primitive.
  ; Primitive procedures can only call other primitive ones.
  ; Non-primitive procedures can call primitive and non-primitive ones.
  ; You can pass values only to non-primitive procedures.
  ; If no value is passed in registers to non-primitive procedure,
  ; 8-bit offset to the top value on the stack is passed as first argument.

  (define-expression (value offset byte tagged-word)
    (fxior
      (fxsll offset 24)
      (fxsll byte 16)
      tagged-word))

  (define-expression (byte-value offset byte)
    (value
      offset
      byte
      (tagged-word byte-tag 0)))

  (define-expression (word-value offset word)
    (value
      offset
      (fxand #xff word)
      (tagged-word word-tag (fxsrl word 8))))

  (define-ops
    ((value-data value)
      (dw (fxand #xffff value))
      (db (fxand #xff (fxsrl value 16))))
    ((pair-data car cdr)
      (value-data car)
      (value-data cdr)))

  (define-proc (box)
    ; Push value on the stack
    (push de)
    (push hl)

    ; EHL - banked pointer, paged-in
    (byte-alloc 3)

    ; Preserve banked pointer on the stack
    (preserve (de hl)
      ; DE = box pointer
      (ex de hl)

      ; HL = box pointer
      (ld hl 4)
      (add hl sp)

      ; Copy value
      (dup 3 (ldi)))

    ; Clean-up stack
    (pop bc)  ; return address
    (pop af)  ; value
    (pop af)
    (push bc) ; return address
    (ret))

  (define-proc (unbox)
    (ld a e)
    (preserve (de) (banked-pointer-page-in a hl))
    (ld c (hl))
    (inc hl)
    (ld b (hl))
    (inc hl)
    (ld e (hl))
    (ld h b)
    (ld l c)
    (ret))

  (define-proc (car)
    (unbox-tc))

  (define-proc (cdr)
    (add hl 3)
    (unbox-tc))

  (define-proc (cons)
    ; Push car on the stack
    (push de)
    (push hl)

    ; EHL - banked pointer, paged-in
    (byte-alloc 6)

    ; Preserve banked pointer on the stack
    (preserve (de hl)
      ; DE = pair pointer
      (ex de hl)

      ; HL = car pointer
      (ld hl 4)
      (add hl sp)

      ; Copy car
      (dup 3 (ldi))

      ; Move HL pointer to cdr
      (add hl 3)

      ; Copy cdr
      (dup 3 (ldi)))

    ; Clean-up stack
    (pop af)  ; car
    (pop af)
    (pop bc)  ; return address
    (pop af)  ; cdr
    (pop af)
    (push bc) ; return address
    (ret))
)
