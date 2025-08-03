(import (zx-next test) (zx-next mem) (zx-next bump-pointer))

(test
  ; Fill bank with garbage
  (ld de #xe000)
  (ld bc #x2000)
  (ld a #xbb)
  (call mem-fill)

  (case init
    (ld e #xe0) ; bank
    (call bump-pointer-init)
    (assert hl #xe000)
    (assert-word (#xe000) #x00bb))

  (case alloc-1
    (ld hl #xe000)
    (ld bc #x00fe)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #xe100)
    (assert de #xe002)
    (assert-word (#xe000) #xa0fe)
    (assert-word (#xe100) #x00bb))

  (case alloc-2
    (ld bc #x00fc)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #xe1fe)
    (assert de #xe102)
    (assert-word (#xe100) #xa0fc)
    (assert-word (#xe1fe) #x00bb))

  (case alloc-zero
    (ld bc #x0000)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #xe200)
    (assert de #xe200)
    (assert-word (#xe1fe) #xa000)
    (assert-word (#xe200) #x00bb))

  (case too-big
    (ld bc #x1dfd)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert c)
    (assert hl #xe200)
    (assert-word (#xe1fe) #xa000))

  (case fill-up
    (ld bc #x1dfc)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert nc)
    (assert hl #xfffe)
    (assert de #xe202)
    (assert-word (#xe200) #xbdfc)
    (assert-word (#xfffe) #x00bb))

  (case zero-block-at-end
    (ld bc #x0000)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert c)
    (assert hl #xfffe))

  (case already-full
    (ld bc #x00fe)
    (ld d #xa0) ; tag
    (ld e #xe0) ; bank
    (call bump-pointer-alloc)
    (assert c)
    (assert hl #xfffe))
)
