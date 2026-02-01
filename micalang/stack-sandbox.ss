(import (micascheme))

(lambda ($mem $sp)
  (define (i32-inv)
    (set-virtual-register! 0 
      (fx+/wraparound (virtual-register 0))))
  (define (i32-add)
    (set-virtual-register! 0 
      (fx+/wraparound (virtual-register 0) (virtual-register 1))))
  )
