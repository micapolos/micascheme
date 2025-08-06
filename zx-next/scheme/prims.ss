(library (zx-next scheme prims)
  (export
    cons
    cons-tc)
  (import
    (zx-next core)
    (zx-next scheme alloc))

  ; DEHL - car, where D = 4 (offset to previous entry on the stack)
  ; stack - cdr (4 bytes)
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
