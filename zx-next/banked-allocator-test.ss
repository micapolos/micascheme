(import (zx-next test) (zx-next banked-allocator))

(define-asm banked-allocator (ds 4))

(test
  (case init
    (banked-allocator-init banked-allocator)
    (ld hl banked-allocator)
    (assert-byte ((+ banked-allocator banked-allocator-first-bank))   #x01)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) #x01))

  ; (case alloc
  ;   (banked-allocator-alloc banked-allocator #x0ffe #xa0)
  ;   (assert nc)
  ;   (assert de #xe002))
)

