(library (asm-2 binary)
  (export
    put-binary
    db-binary
    dw-binary
    binary-append
    binary->bytevector)
  (import (micascheme) (asm-2 u))

  (define (put-binary $port $binary)
    ($binary $port))

  (define-rule-syntax (db-binary $db)
    (lambda ($port)
      (put-u8 $port (u8 $db))))

  (define-rule-syntax (dw-binary $dw)
    (lambda ($port)
      (put-u16 $port (u16 $dw) 'little)))

  (define (binary-append . $binaries)
    (lambda ($port)
      (for-each
        (partial put-binary $port)
        $binaries)))

  (define (binary->bytevector $binary)
    (call-with-bytevector-output-port
      (lambda ($port)
        (put-binary $port $binary))))
)
