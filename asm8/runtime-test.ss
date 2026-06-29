(import (scheme) (check) (asm8 runtime) (lets) (procedure))

(check (u8= 10 10))

(lets
  (mem (make-bytevector 2 0))
  (run
    (check (u8= (u8-ref 0) 0))
    (check (u8= (u8-ref 1) 0))
    (u8-set! 0 #x12)
    (check (u8= (u8-ref 0) #x12))
    (check (u8= (u8-ref 1) 0))))

