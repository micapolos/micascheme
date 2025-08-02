(library (zx-next bank)
  (export
    banks-init
    bank-alloc
    bank-free
    write-banks)
  (import
    (zx-next core)
    (zx-next debug)
    (zx-next write))

  (define-fragments
    (available-string (dz "Available banks: ")))

  (define-values
    (bank-status-free 0)
    (bank-status-system 1)
    (bank-status-unavailable #xff))

  (define-fragments
    (bank-status-map
      (ds 1     bank-status-system)
      (ds 223   bank-status-free)
      (ds 32    bank-status-unavailable))
    (bank-current (db 0))
    (banks-free (db 223)))

  ; TODO: Initalize according to available memory (how to detect it?)
  (define-asm banks-init
    (ret))

  (define-asm bank-index->addr
    (input (a bank-index))
    (output (hl addr))
    (ld hl bank-status-map)
    (add hl a)
    (ret))

  (define-asm bank-alloc
    (output (a non-zero type) (fc out-of-memory))

    ; e - status
    (ld e a)

    ; check out of memory
    (ld a (banks-free))
    (or a)
    (when z (scf) (ret))

    (loop
      (ld hl bank-current)
      (inc (hl))
      (ld a (hl))
      (call bank-index->addr)
      (ld a (hl))
      (or a)
      (when z
        (ld (hl) e)
        (ld hl banks-free)
        (dec (hl))
        (rcf)
        (ret))))

  (define-asm bank-free
    (input (a bank-index))

    (call bank-index->addr)
    (ld a (hl))
    (or a)
    (ret z)

    (ld (hl) 0)
    (ld hl banks-free)
    (inc (hl))
    (ret))

  (define-asm write-banks
    (ld hl available-string)
    (call write-string)
    (ld a (banks-free))
    (call write-byte)
    (call write-newline)

    (dump bank-status-map #x100)
    (ret))
)
