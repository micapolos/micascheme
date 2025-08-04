(import (zx-next test) (zx-next banked-allocator))

(define-asm banked-allocator (ds 4))

(test
  (case init
    (banked-allocator-init banked-allocator)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) #xff)
    (assert-word ((+ banked-allocator banked-allocator-allocator)) #x0000))

  (case alloc-00fe
    (break)
    (banked-allocator-alloc banked-allocator #x00fe #xa0)
    (assert nc)
    (assert de #xe002)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) 0)
    (assert-word ((+ banked-allocator banked-allocator-allocator)) #xe100)
    (assert-byte ((+ banked-allocator banked-allocator-banks 0)) 1))

  ; (case alloc-00fe
  ;   (banked-allocator-alloc banked-allocator #x00fe #xa0)
  ;   (assert nc)
  ;   (assert de #xe102)
  ;   (assert-byte ((+ banked-allocator banked-allocator-current-bank)) 0)
  ;   (assert-word ((+ banked-allocator banked-allocator-allocator)) #xe200)
  ;   (assert-byte ((+ banked-allocator banked-allocator-banks 0)) 1))
)

