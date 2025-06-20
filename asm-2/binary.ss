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

  (define (db-binary $db $syntax)
    (lambda ($port)
      (put-u8 $port (u8 $db $syntax))))

  (define (dw-binary $dw $syntax)
    (lambda ($port)
      (put-u16 $port
        (u16 $dw $syntax)
        (endianness little))))

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
