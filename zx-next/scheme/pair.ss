(library (zx-next scheme pair)
  (export car cdr)
  (import
    (zx-next core)
    (zx-next scheme value))

  ; Pair is stored aligned in memory, so switching between car and cdr pointers
  ; involves changing bit 2 of register L.

  ; Dereferences car of a pair.
  (proc car
    (jp ref))

  ; Dereferences cdr of a pair.
  (proc cdr
    (set 2 l)
    (jp ref))
)
