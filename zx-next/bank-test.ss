(import (zx-next test) (zx-next terminal) (zx-next bank))

(test
  (case init
    (call banks-init))

  (case free-banks
    (load-free-banks a)
    (assert a #xd0))

  (case free-bank-0?
    (ld a #x00)
    (call bank-free?)
    (assert nz))

  (case free-bank-0f?
    (ld a #x0f)
    (call bank-free?)
    (assert nz))

  (case free-bank-10?
    (ld a #x10)
    (call bank-free?)
    (assert z))

  (case free-bank-df?
    (ld a #xd0)
    (call bank-free?)
    (assert z))

  (case free-bank-e0?
    (ld a #xe0)
    (call bank-free?)
    (assert nz))

  (case alloc
    (ld a #x34)  ; some type
    (call bank-alloc)
    (assert nc)
    (assert a #x10))

  (case free-banks
    (load-free-banks a)
    (assert a #xcf))

  (case free-bank-1?
    (ld a #x01)
    (call bank-free?)
    (assert nz))

  (case alloc
    (ld a #x34)  ; some type
    (call bank-alloc)
    (assert nc)
    (assert a #x11))

  (case free-banks
    (load-free-banks a)
    (assert a #xce))

  (case free-bank-2?
    (ld a #x02)
    (call bank-free?)
    (assert nz))
)
