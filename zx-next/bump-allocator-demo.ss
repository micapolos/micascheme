(import (zx-next demo) (zx-next bump-allocator))

(define-fragment bump-allocator (bump-allocator-data))

(demo
  (ld hl bump-allocator)
  (call bump-allocator-init)


  (ld hl bump-allocator)
  (ld bc #x1234)
  (call bump-allocator-alloc)
  (break))
