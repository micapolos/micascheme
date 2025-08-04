(import (zx-next test) (zx-next terminal) (zx-next bank) (zx-next mem) (zx-next mmu))

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

  (case alloc-all
    (ld b 0)
    (loop
      (ld a #x34)
      (preserve (bc) (call bank-alloc))
      (if nc
        (then
          (mmu 7 a)
          (preserve (bc)
            (write #\.)
            (mem-fill #xe000 #x2000 #xbb))
          (inc b)
          (rcf))
        (else
          (preserve (bc) (write #\!))
          (scf)))
        (while nc))
    (writeln "\rAllocated " b " banks."))

  ; TODO
  (case free-all)
)
