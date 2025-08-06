(library (zx-next scheme prims)
  (export
    value
    value-data
    pair-data
    unbox unbox-tc
    car car-tc
    cdr cdr-tc
    cons cons-tc)
  (import
    (zx-next core)
    (zx-next scheme alloc)
    (zx-next banked-pointer))

  ; All procedures use __sdcccall(1) calling convention.
  ; If no value is passed in registers, offset to the value is passed in A.

  (define-expression (value offset n mm)
    (fxior (fxsll n 16) mm))

  (define-ops
    ((value-data value)
      (dw (fxand #xffff value))
      (db (fxsrl value 16)))
    ((pair-data car cdr)
      (value-data car)
      (value-data cdr)))

  (define-proc (unbox)
    (ld a e)
    (preserve (de) (banked-pointer-page-in a hl))
    (ld c (hl))
    (inc hl)
    (ld b (hl))
    (inc hl)
    (ld e (hl))
    (ld h c)
    (ld l b)
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
