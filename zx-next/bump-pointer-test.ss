(import (zx-next test) (zx-next mem) (zx-next bump-pointer) (zx-next tagged))

(test
  (mem-fill #xe000 #x2000 #xbb)

  (case init
    (bump-pointer-init #xe000)
    (assert-word (#xe000) #x00bb))

  (case alloc-1
    (bump-pointer-alloc #xe000 (tagged-word #xa0 #x00fe))
    (assert nc)
    (assert hl #xe100)
    (assert de #xe002)
    (assert-word (#xe000) #xa0fe)
    (assert-word (#xe100) #x00bb))

  (case alloc-2
    (bump-pointer-alloc #xe100 (tagged-word #xa0 #x00fc))
    (assert nc)
    (assert hl #xe1fe)
    (assert de #xe102)
    (assert-word (#xe100) #xa0fc)
    (assert-word (#xe1fe) #x00bb))

  (case alloc-zero
    (bump-pointer-alloc #xe1fe (tagged-word #xa0 #x0000))
    (assert nc)
    (assert hl #xe200)
    (assert de #xe200)
    (assert-word (#xe1fe) #xa000)
    (assert-word (#xe200) #x00bb))

  (case too-big
    (bump-pointer-alloc #xe200 (tagged-word #xa0 #x1dfd))
    (assert c)
    (assert hl #xe200)
    (assert-word (#xe1fe) #xa000))

  (case fill-up
    (bump-pointer-alloc #xe200 (tagged-word #xa0 #x1dfc))
    (assert nc)
    (assert hl #xfffe)
    (assert de #xe202)
    (assert-word (#xe200) #xbdfc)
    (assert-word (#xfffe) #x00bb))

  (case zero-block-at-end
    (bump-pointer-alloc #xfffe (tagged-word #xa0 #x0000))
    (assert c)
    (assert hl #xfffe))

  (case already-full
    (bump-pointer-alloc #xfffe (tagged-word #xa0 #x00fe))
    (assert c)
    (assert hl #xfffe))
)
