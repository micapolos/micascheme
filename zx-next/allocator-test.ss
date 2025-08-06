(import (zx-next test) (zx-next mem) (zx-next allocator) (zx-next tagged))

(define-asm allocator (ds 2))

(test
  (mem-fill #xe000 #x2000 #xbb)

  (case init
    (ld hl allocator)
    (allocator-init allocator)
    (assert-word (allocator) #xe000)
    (assert-word (#xe000) #x00bb))

  (case alloc
    (allocator-alloc allocator (tagged-word #xa0 #x0100))
    (assert nc)
    (assert de #xe002)
    (assert-word (allocator) #xe102)
    (assert-word (#xe000) #xa100)
    (assert-word (#xe102) #x00bb))

  (case overflow
    (allocator-alloc allocator (tagged-word #xa0 #x1ff0))
    (assert c)
    (assert-word (allocator) #xe102))

  (case fill-up
    (loop
      (allocator-alloc allocator (tagged-word #xa0 #x0006))
      (preserve (af) (write #\.))
      (while nc))
    (writeln)))
