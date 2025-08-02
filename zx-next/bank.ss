(library (zx-next bank)
  (export
    banks-init
    bank-alloc
    write-banks)
  (import
    (zx-next core)
    (zx-next debug)
    (zx-next write))

  (define-fragments
    (available-string (dz "Available banks: ")))

  (define-fragments
    (bank-alloc-map
      (db #x80)
      (ds 27 0)
      (ds 4 #xff))
    (bank-index (db 0))
    (banks-free (db 223)))

  ; TODO: Initalize according to available memory (how to detect it?)
  (define-asm banks-init
    (ret))

  (define-asm bank-index->addr/mask
    (input (a bank-index))
    (output (hl addr) (e mask))
    (ld c a)

    ; E = mask
    (ld de #x0080)
    (and #x07)
    (ld b a)
    (bsrl de b)

    (ld hl bank-alloc-map)
    (ld a c)
    (dup 3 (rrca))
    (add hl a)
    (ret))

  (define-asm bank-alloc
    (output (a bank) (fc out-of-memory))

    ; check out of memory
    (ld a (banks-free))
    (or a)
    (when z (scf) (ret))

    (loop
      (call bank-index->addr/mask)
      (ld a e)
      (and a)
      ))

  (define-asm write-banks
    (ld hl available-string)
    (call write-string)
    (ld a (banks-free))
    (call write-byte)
    (call write-newline)

    (dump bank-alloc-map #x20)
    (ret))
)
