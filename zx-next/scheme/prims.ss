(library (zx-next scheme prims)
  (export
    offset/byte
    value

    load-value
    load-null
    load-void
    load-false
    load-true

    null-value
    void-value
    false-value
    true-value
    byte-value
    word-value
    char-value
    string-value
    symbol-value
    pair-value
    value-data
    pair-data
    box box-tc

    unsafe-unbox unsafe-unbox-tc
    unsafe-car unsafe-car-tc
    unsafe-cdr unsafe-cdr-tc

    car car-tc
    cdr cdr-tc
    cons cons-tc

    null? null?-tc
    byte? byte?-tc
    word? word?-tc
    pair? pair?-tc
    char? char?-tc
    string? string?-tc
    symbol? symbol?-tc

    throw)
  (import
    (zx-next core)
    (zx-next scheme alloc)
    (zx-next banked-pointer)
    (zx-next tag)
    (zx-next tagged)
    (zx-next dispatch)
    (zx-next scheme tag)
    (zx-next scheme constant)
    (rename (zx-next throw)
      (throw zx-throw)))

  ; All procedures use __sdcccall(1) calling convention.
  ; There are two types of procedures: primitive and non-primitive.
  ; Primitive procedures can only call other primitive ones.
  ; Non-primitive procedures can call primitive and non-primitive ones.
  ; You can pass values only to non-primitive procedures.
  ; If no value is passed in registers to non-primitive procedure,
  ; 8-bit offset to the top value on the stack is passed as first argument.

  (define-expression (offset/byte offset byte)
    (fxior (fxsll offset 8) byte))

  (define-expression (value offset byte tagged-word)
    (fxior
      (fxsll offset 24)
      (fxsll byte 16)
      tagged-word))

  (define-expression (null-value offset)
    (value
      offset
      0
      (fxsll null-constant 8)))

  (define-expression (void-value offset)
    (value
      offset
      0
      (fxsll void-constant 8)))

  (define-expression (false-value offset)
    (value
      offset
      0
      (fxsll false-constant 8)))

  (define-expression (true-value offset)
    (value
      offset
      0
      (fxsll true-constant 8)))

  (define-expression (byte-value offset byte)
    (value
      offset
      byte
      byte-constant-word))

  (define-expression (word-value offset word)
    (value
      offset
      (fxand #xff word)
      (fxior word-constant-word (fxsrl word 8))))

  (define-expression (char-value offset char)
    (value
      offset
      char
      char-constant-word))

  (define-expression (string-value offset address)
    (value
      offset
      (fxand #xff address)
      (fxior (fxsll string-constant 8) (fxsrl address 8))))

  (define-expression (symbol-value offset address)
    (value
      offset
      (fxand #xff address)
      (fxior (fxsll symbol-constant 8) (fxsrl address 8))))

  (define-expression (pair-value offset address)
    (value
      offset
      0
      (tagged-word pair-tag (fxand #x1fff address))))

  (define-ops
    ((value-data value)
      (dw (fxand #xffff value))
      (db (fxand #xff (fxsrl value 16))))
    ((pair-data car cdr)
      (value-data car)
      (value-data cdr))
    ((load-value value)
      (ld de (fxsrl value 16))
      (ld hl (fxand value #xffff))))

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

  (define-proc (load-null d)
    (input (d offset))
    (ld e 0)
    (ld hl null-constant-word)
    (ret))

  (define-proc (load-void d)
    (input (d offset))
    (ld e 0)
    (ld hl void-constant-word)
    (ret))

  (define-proc (load-false d)
    (input (d offset))
    (ld e 0)
    (ld hl false-constant-word)
    (ret))

  (define-proc (load-true d)
    (input (d offset))
    (ld e 0)
    (ld hl true-constant-word)
    (ret))

  (define-proc (ref)
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

  (define-op (ensure-tagged? tag)
    (ld a h)
    (and tag-mask)
    (cp tag)
    (when nz (zx-throw)))

  (define-proc (unsafe-unbox)
    (ref-tc))

  (define-proc (unsafe-car)
    (ref-tc))

  (define-proc (unsafe-cdr)
    (add hl 3)
    (ref-tc))

  (define-proc (car)
    (ensure-tagged? pair-tag)
    (unsafe-car-tc))

  (define-proc (cdr)
    (ensure-tagged? pair-tag)
    (unsafe-cdr-tc))

  (define-op (load-tagged? tag)
    (ld a h)
    (and tag-mask)
    (cp tag)
    (if z
      (then (load-true d))
      (else (load-false d))))

  (define-op (load-constant? constant)
    (ld a h)
    (cp (tagged-byte constant-tag constant))
    (when z
      (load-true d)
      (ret))
    (load-false d))

  (define-proc (null?)
    (load-constant? null-constant)
    (ret))

  (define-proc (byte?)
    (load-constant? byte-constant)
    (ret))

  (define-proc (word?)
    (load-constant? word-constant)
    (ret))

  (define-proc (pair?)
    (load-tagged? pair-tag)
    (ret))

  (define-proc (char?)
    (load-constant? char-constant)
    (ret))

  (define-proc (string?)
    (load-constant? string-constant)
    (ret))

  (define-proc (symbol?)
    (load-constant? symbol-constant)
    (ret))

  (define-proc (throw)
    (zx-throw))

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
