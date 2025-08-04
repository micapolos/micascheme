(import (zx-next test) (zx-next mem) (zx-next bump-allocator))

(define-asm bump-allocator (ds 2))

(test
  (mem-fill #xe000 #x2000 #xbb)

  (case init
    (ld hl bump-allocator)
    (bump-allocator-init bump-allocator)
    (assert-word (bump-allocator) #xe000)
    (assert-word (#xe000) #x00bb))

  (case alloc
    (bump-allocator-alloc bump-allocator #x100 #xa0)
    (assert nc)
    (assert de #xe002)
    (assert-word (bump-allocator) #xe102)
    (assert-word (#xe000) #xa100)
    (assert-word (#xe102) #x00bb))

  (case overflow
    (bump-allocator-alloc bump-allocator #x1ff0 #xa0)
    (assert c)
    (assert-word (bump-allocator) #xe102)))
