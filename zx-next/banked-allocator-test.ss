(import (zx-next test) (zx-next banked-allocator))

(define-asm banked-allocator (ds 4))

(test
  (case init
    (banked-allocator-init banked-allocator)
    (assert-byte (banked-allocator) #x01)          ; current-bank
    (assert-byte ((+ banked-allocator 3)) #x01)))  ; first bank
