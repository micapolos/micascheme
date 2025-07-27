(library (zx-next scheme pair)
  (export car cdr)
  (import
    (zx-next core)
    (zx-next scheme value))

  ; Pair is stored aligned in memory, so switching between car and cdr pointers
  ; involves changing bit 2 of register L.

  (block heap-bank
    (db #x01))

  (block heap-top
    (dw #x0000))

  ; Dereferences car of a pair.
  (block car
    (jp ref))

  ; Dereferences cdr of a pair.
  (block cdr
    (set 2 l)
    (jp ref))

  (block pair-alloc
    (ld hl (heap-top)))
)
