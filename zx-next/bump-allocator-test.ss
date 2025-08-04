(import (zx-next test) (zx-next mem) (zx-next bump-allocator))

(define-asm bump-allocator (ds 2))

(test
  (mem-fill #xe000 #x2000 #xbb)

  (case init
    (bump-allocator-init bump-allocator)
    (assert-word (bump-allocator) #xe000)
    (assert-word (#xe000) #x00bb))

  (case alloc
    (bump-allocator-alloc bump-allocator #x100 #xa0)
    (assert nc)
    (assert de #xe002)
    (assert-word (bump-allocator) #xe102)
    (assert-word (#xe000) #xa100)))
