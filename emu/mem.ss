(library (emu mem)
  (export define-mem)
  (import (scheme) (syntax) (syntaxes) (lets) (emu math))

  (define-rule-syntax (define-mem $id $size)
    (begin
      (define $bytevector (make-bytevector $size 0))
      (define-rules-syntax
        (($id $addr) (bytevector-u8-ref $bytevector $addr))
        (($id $addr $u8) (bytevector-u8-set! $bytevector $addr $u8)))))
)
