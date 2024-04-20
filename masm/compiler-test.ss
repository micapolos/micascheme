(import (micascheme) (masm compiler))

(define-op (const $u8)
  (op ($bytevector $sp)
    (set! $sp (fxand (sub1 $sp) #xff))
    (bytevector-u8-set! $bytevector $sp $u8)))

(define-op (add)
  (op ($bytevector $sp)
    (lets
      ($rhs (bytevector-u8-ref $bytevector $sp))
      (run (set! $sp (add1 $sp)))
      ($lhs (bytevector-u8-ref $bytevector $sp))
      (bytevector-u8-set! $bytevector $sp (fxand (+ $lhs $rhs) #xff)))))

(define-op (sub)
  (op ($bytevector $sp)
    (lets
      ($rhs (bytevector-u8-ref $bytevector $sp))
      (run (set! $sp (add1 $sp)))
      ($lhs (bytevector-u8-ref $bytevector $sp))
      (bytevector-u8-set! $bytevector $sp (fxand (- $lhs $rhs) #xff)))))

(lets
  ($run
    (masm
      (const #x30)
      (const #x20)
      (const #x01)
      (add)
      (sub)))
  ($bytevector (make-bytevector #x3))
  (run-void
    ($run $bytevector)
    (check (equal? $bytevector #vu8(#x01 #x21 #x0f)))))
