(import (micascheme))

(lets
  ($size 102400000)
  ($bytevector (make-bytevector $size))
  ($sum 0)
  (run
    (repeat-indexed ($index $size) (bytevector-u8-set! $bytevector $index (random 256))))
    (time
      (repeat-indexed ($index $size)
        (lets
          ($u8 (bytevector-u8-ref $bytevector $index))
          (cond
            ((fx= $u8 0) (set! $sum (fx+/wraparound $sum 12)))
            ((fx= $u8 1) (set! $sum (fx+/wraparound $sum 123)))
            ((fx= $u8 2) (set! $sum (fx+/wraparound $sum 121)))
            ((fx= $u8 3) (set! $sum (fx+/wraparound $sum 124)))
            ((fx= $u8 4) (set! $sum (fx+/wraparound $sum 121)))
            ((fx= $u8 5) (set! $sum (fx+/wraparound $sum 122)))
            ((fx= $u8 6) (set! $sum (fx+/wraparound $sum 126)))
            ((fx= $u8 7) (set! $sum (fx+/wraparound $sum 122)))
            ((fx= $u8 8) (set! $sum (fx+/wraparound $sum 126)))
            (else (set! $sum (fx+/wraparound $sum 143))))))))
