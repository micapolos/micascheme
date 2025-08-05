(library (zx-next bank-table)
  (export
    bank-table

    init-bank-cursor
    inc-bank-cursor

    bank-reserved?
    bank-reserve
    bank-free
    count-free-banks

    write-bank-table)
  (import (zx-next core) (zx-next debug))

  (define-fragments
    ; bank #x00 - code
    ; bank #x05 - stack
    ; bank #x0A, #x0B - ULA
    ; last 32 banks - unavailable
    (bank-table
      (align #x20)
      (db
        #b10000100 #b00110000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
        #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
        #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
        #b00000000 #b00000000 #b00000000 #b00000000 #b11111111 #b11111111 #b11111111 #b11111111)))

  ; Cursor:
  ; - DE = mask/index
  ; - HL = address in table
  (define-ops (keywords de hl)
    ((init-bank-cursor)
      (ld de #x8000)
      (ld hl bank-table))

    ((inc-bank-cursor de hl)
      (inc e)
      (rrc d)
      (when c
        (inc l)
        (ld a l)
        (and #x1f)
        (or (fxand bank-table #xe0))
        (ld l a))))

  (define-proc (load-bank-index e)
    (output (hl address) (a mask))
    (ld hl bank-table)
    (ld a e)
    (rrca)
    (rrca)
    (rrca)
    (and #x1f)
    (add hl a)
    (setae)
    (ret))

  (define-proc (bank-reserved? e)
    (load-bank-index e)
    (and (hl))
    (ret))

  (define-proc (bank-reserve e)
    (load-bank-index e)
    (or (hl))
    (ld (hl) a)
    (ret))

  (define-proc (bank-free e)
    (load-bank-index e)
    (cpl)
    (and (hl))
    (ld (hl) a)
    (ret))

  (define-proc (count-free-banks)
    (output (a free-banks))

    ; Init cursor in DE HL
    (init-bank-cursor)

    ; B = repeat counter
    ; C = free bank counter
    (ld bc 0)

    ; Repeat #x100 times
    (loop-djnz
      ; Load mask
      (ld a d)

      ; Check bank status
      (and (hl))

      ; Increment counter if free
      (when z (inc c))

      ; Increment cursor
      (inc-bank-cursor de hl))

    ; Load free bank count in a
    (ld a c)

    (ret))

  (define-proc (write-bank-table)
    (dump bank-table #x20)
    (ret))
)
