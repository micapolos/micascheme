(library (zx-next bank)
  (export
    banks-init
    bank-fill
    bank-alloc
    bank-dealloc
    bank-free
    bank-free?
    write-banks
    load-free-banks)
  (import
    (zx-next core)
    (zx-next debug)
    (zx-next write)
    (zx-next mem)
    (zx-next mmu))

  (define-values
    (slot      7)
    (base-addr #xe000)
    (bank-size #x2000))

  (define-proc (bank-fill a l)
    (mmu slot a)
    (ld de base-addr)
    (ld bc bank-size)
    (ld a l)
    (mem-fill de bc a)
    (ret))

  (define-fragments
    (free-string (dz "free ")))

  (define-values
    (bank-status-free 0)
    (bank-status-system 1)
    (bank-status-unavailable #xff))

  (define-fragments
    (bank-status-map
      (ds 16    bank-status-system)
      (ds 208   bank-status-free)
      (ds 32    bank-status-unavailable))
    (bank-current (db 0))
    (banks-free (db 208)))

  ; TODO: Initalize according to available memory (how to detect it?)
  (define-asm banks-init
    (ret))

  (define-asm bank-index->addr
    (input (a bank-index))
    (output (hl addr))
    (ld hl bank-status-map)
    (add hl a)
    (ret))

  (define-ops (keywords a)
    ((load-free-banks a)
      (ld a (banks-free))))

  (define-asm bank-free?
    (input (a bank-index))
    (output (z free?))
    (call bank-index->addr)
    (ld a (hl))
    (or a)
    (ret))

  (define-asm bank-alloc
    (input (a non-zero bank type))
    (output (a bank index) (fc out-of-memory))

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
        (ld a (bank-current))
        (rcf)
        (ret))))

  (define-asm bank-dealloc
    (call bank-index->addr)
    (ld a (hl))
    (or a)
    (ret z)
    (ld (hl) 0)
    (ld hl banks-free)
    (inc (hl))
    (ret))

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
    (ld a (banks-free))
    (writeln "free: " a)

    (ld a 0)
    (loop-byte 4
      (write a #\space)
      (loop-byte 64
        (preserve (af)
          (call bank-free?)
          (if z
            (then (ld a #\.))
            (else (ld a #\X)))
          (call write-char))
        (inc a))
      (preserve (af) (writeln)))
    (ret))
)
