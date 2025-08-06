(import
  (zx-next test)
  (zx-next banked-allocator)
  (zx-next tagged)
  (zx-next bank-table)
  (zx-next mem))

(define-asm banked-allocator (ds 4))

(test
  (case init
    (banked-allocator-init banked-allocator #xe000)
    (assert-byte ((+ banked-allocator 0)) #xff)
    (assert-word ((+ banked-allocator 1)) #xe000))

  (case alloc-new-bank
    (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x00fd))
    (assert nc)
    (assert a #x01)
    (assert de #xe003)
    (assert-byte ((+ banked-allocator 0)) #x01)
    (assert-word ((+ banked-allocator 1)) #xe100))

  (case alloc-same-bank
    (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x10fe))
    (assert nc)
    (assert a #x01)
    (assert de #xe102)
    (assert-byte ((+ banked-allocator 0)) #x01)
    (assert-word ((+ banked-allocator 1)) #xf200))

  (case alloc-new-bank
    (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x0ffd))
    (assert nc)
    (assert a #x02)
    (assert de #xe003)
    (assert-byte ((+ banked-allocator 0)) 2)
    (assert-word ((+ banked-allocator 1)) #xf000))

  (write-bank-table)

  ; It should be possible to allocate 80 * 20 * 510 ~= 800KB
  (case alloc-fill-up
    (ld bc 0)
    (loop
      (preserve (bc)
        (banked-allocator-alloc banked-allocator (tagged-word #xa0 #x0400)))

      (when nc
        (preserve (bc)
          (ld bc #x0400)
          (ld a #xbb)
          (mem-fill de bc a)
          (write #\.))

        ; Increment alloc counter
        (inc bc)

        (rcf))
      (while nc))
    (writeln "\rallocated " bc " banks of #x400 bytes"))

  (write-bank-table)
)

