(import (micascheme) (masm compiler) (u8))

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
      (bytevector-u8-set! $bytevector $sp (u8+ $lhs $rhs)))))

(define-op (sub)
  (op ($bytevector $sp)
    (lets
      ($rhs (bytevector-u8-ref $bytevector $sp))
      (run (set! $sp (add1 $sp)))
      ($lhs (bytevector-u8-ref $bytevector $sp))
      (bytevector-u8-set! $bytevector $sp (u8- $lhs $rhs)))))

(define-rule-syntax (check-masm ($in ...) $op ... ($out ...))
  (let ()
    (define $bytevector-in (bytevector $in ...))
    (define $bytevector-out (bytevector $out ...))
    (app (masm $op ...) $bytevector-in)
    (check (equal? $bytevector-in $bytevector-out))))

(check-masm
  (#x00 #x00 #x00)
  (const #x30)
  (const #x20)
  (const #x01)
  (add)
  (sub)
  (#x01 #x21 #x0f))
