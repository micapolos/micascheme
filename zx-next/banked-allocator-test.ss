(import
  (zx-next test)
  (zx-next banked-allocator)
  (zx-next tagged))

(define-asm banked-allocator (ds 4))

(test
  (case init
    (banked-allocator-init banked-allocator)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) #xff)
    (assert-word ((+ banked-allocator banked-allocator-allocator)) #xffff))

  (case alloc-00fe
    (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x00fe))
    (assert nc)
    (assert de #xe002)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) 0)
    (assert-word ((+ banked-allocator banked-allocator-allocator)) #xe100)
    (assert-byte ((+ banked-allocator banked-allocator-banks 0)) #x10))

  (case alloc-10fe
    (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x10fe))
    (assert nc)
    (assert de #xe102)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) 0)
    (assert-word ((+ banked-allocator banked-allocator-allocator)) #xf200)
    (assert-byte ((+ banked-allocator banked-allocator-banks 0)) #x10))

  (case alloc-0ffe
    (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x0ffe))
    (assert nc)
    (assert de #xe002)
    (assert-byte ((+ banked-allocator banked-allocator-current-bank)) 1)
    (assert-word ((+ banked-allocator banked-allocator-allocator)) #xf000)
    (assert-byte ((+ banked-allocator banked-allocator-banks 0)) #x10)
    (assert-byte ((+ banked-allocator banked-allocator-banks 1)) #x11))

  ; It should be possible to allocate 80 * 20 * 510 ~= 800MB
  (case bulk-alloc-2000-6
    (loop-word (* 80 20)
      (write #\.)
      (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x01fe))
      (assert nc)))
)

