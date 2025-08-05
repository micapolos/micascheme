(library (zx-next bank-alloc)
  (export bank-alloc)
  (import
    (zx-next core)
    (zx-next bank-table))

  (define-fragment cursor
    ; index
    (db 0)
    ; mask
    (db #x80)
    ; table address LSB
    (db (fxand bank-table #xff)))

  (define-proc (load-bank-index hl e)
    (ld hl bank-table)
    (ld a e)
    (rrca)
    (rrca)
    (rrca)
    (and #x1f)
    (add hl a)
    (setae)
    (ret))

  (define-proc (load-cursor)
    (output (de mask/index) (hl address))
    (ld hl cursor)
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (inc hl)
    (ld l (hl))
    (ld h (fxsrl bank-table 8))
    (ret))

  (define-proc (save-cursor)
    (output (de mask/index) (hl address))
    (ld a l)
    (ld hl cursor)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (inc hl)
    (ld (hl) a)
    (ret))

  (define-proc (bank-alloc)
    (output
      (fc = 0 ok / 1 error)
      (a = allocated bank index))

    ; DE = mask/index
    ; HL = address
    (load-cursor)

    ; Repeat #x100 times until free bank is found, and reserve it.
    (ld b 0)
    (loop-djnz
      ; Check bank is reserved
      (ld a d)
      (and (hl))

      ; If not, reserve and return
      (when z
        (ld a d)
        (or (hl))
        (ld (hl) a)

        (save-cursor)
        (ld a e)
        (ret))

      (inc-bank-cursor de hl))

    ; Out of banks
    (scf)
    (ret))
)
