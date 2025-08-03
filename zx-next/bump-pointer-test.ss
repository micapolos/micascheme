(import (zx-next test) (zx-next bump-pointer))

(test
  (case alloc-1
    (ld hl #xe000)
    (ld bc #x00fe)
    (ld d #b10100000) ; tag
    (ld e #b11100000) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #xe100)
    (assert de #xe002))

  (case alloc-2
    (ld bc #x00fe)
    (ld d #b10100000) ; tag
    (ld e #b11100000) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #xe200)
    (assert de #xe102))

  (case too-big
    (ld bc #x1dff)
    (ld d #b10100000) ; tag
    (ld e #b11100000) ; bank
    (call bump-pointer-alloc)
    (assert c)
    (assert hl #xe200))

  (case fill-up
    (ld bc #x1dfe)
    (ld d #b10100000) ; tag
    (ld e #b11100000) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #x0000)
    (assert de #xe202))

  (case already-full
    (ld bc #x00fe)
    (ld d #b10100000) ; tag
    (ld e #b11100000) ; bank
    (call bump-pointer-alloc)
    (assert c)
    (assert hl #x0000))
)
