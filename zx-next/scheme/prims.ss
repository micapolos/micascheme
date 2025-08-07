(library (zx-next scheme prims)
  (export
    load-value
    load-null
    load-void
    load-false
    load-true

    push-value

    value-data
    pair-data

    box
    box-tc
    box-proc

    void
    void-tc
    void-proc

    put-char
    put-char-tc
    put-char-proc

    unsafe-put-char
    unsafe-put-char-tc
    unsafe-put-char-proc

    put-string
    put-string-tc
    put-string-proc

    unsafe-put-string
    unsafe-put-string-tc
    unsafe-put-string-proc

    unsafe-unbox
    unsafe-unbox-tc
    unsafe-unbox-proc

    unsafe-car
    unsafe-car-tc
    unsafe-car-proc

    unsafe-cdr
    unsafe-cdr-tc
    unsafe-cdr-proc

    car
    car-tc
    car-proc

    cdr
    cdr-tc
    cdr-proc

    cons
    cons-tc
    cons-proc

    null?
    null?-tc
    null?-proc

    byte?
    byte?-tc
    byte?-proc

    word?
    word?-tc
    word?-proc

    pair?
    pair?-tc
    pair?-proc

    char?
    char?-tc
    char?-proc

    string?
    string?-tc
    string?-proc

    symbol?
    symbol?-tc
    symbol?-proc

    throw
    throw-tc
    throw-proc

    unsafe-apply
    unsafe-apply-tc
    unsafe-apply-proc)
  (import
    (zx-next core)
    (zx-next scheme alloc)
    (zx-next banked-pointer)
    (zx-next tag)
    (zx-next tagged)
    (zx-next dispatch)
    (zx-next scheme tag)
    (zx-next scheme constant)
    (zx-next scheme value)
    (prefix (zx-next write) zx-)
    (rename (zx-next throw)
      (throw zx-throw)))

  ; All procedures use __sdcccall(1) calling convention.
  ; There are two types of procedures: primitive and non-primitive.
  ; Primitive procedures can only call other primitive ones.
  ; Non-primitive procedures can call primitive and non-primitive ones.
  ; You can pass values only to non-primitive procedures.
  ; If no value is passed in registers to non-primitive procedure,
  ; 8-bit offset to the top value on the stack is passed as first argument.

  (define-ops
    ((value-data value)
      (dw (fxand #xffff value))
      (db (fxand #xff (fxsrl value 16))))
    ((pair-data car cdr)
      (value-data car)
      (value-data cdr))
    ((load-value value)
      (ld de (fxsrl value 16))
      (ld hl (fxand value #xffff)))
    ((push-value value)
      (load-value value)
      (push de)
      (push hl)))

  (define-op (ensure-tagged? tag)
    (ld a h)
    (and tag-mask)
    (cp tag)
    (when nz (zx-throw)))

  (define-op (ensure-constant? constant)
    (ld a h)
    (cp constant)
    (when nz (zx-throw)))

  (define-op (pointer-ref de hl)
    (input (de hl value))
    (output (mmu paged-in) (hl address) (de preserved))
    (ld a e)
    (preserve (de) (banked-pointer-page-in a hl))
    (ld h l)
    (ld l d))

  ; D = offset
  (define-proc (void)
    (ld e 0)
    (ld hl (constant-word void-constant))
    (ret))

  (define-proc (unsafe-put-char)
    (preserve (de) (ld a e) (call zx-write-char))
    (ld e 0)
    (ld hl (constant-word void-constant))
    (ret))

  (define-proc (put-char)
    (ensure-constant? char-constant)
    (unsafe-put-char-tc))

  (define-proc (unsafe-put-string)
    (preserve (de)
      (ld h l)
      (ld l e)
      (call zx-write-string))
    (ld e 0)
    (ld hl (constant-word void-constant))
    (ret))

  (define-proc (put-string)
    (ensure-constant? string-constant)
    (unsafe-put-string-tc))

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
    (ld hl (constant-word null-constant))
    (ret))

  (define-proc (load-void d)
    (input (d offset))
    (ld e 0)
    (ld hl (constant-word void-constant))
    (ret))

  (define-proc (load-false d)
    (input (d offset))
    (ld e 0)
    (ld hl (constant-word false-constant))
    (ret))

  (define-proc (load-true d)
    (input (d offset))
    (ld e 0)
    (ld hl (constant-word true-constant))
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

  (define-proc (unsafe-apply)
    (pointer-ref de hl)
    (jp (hl)))
)
