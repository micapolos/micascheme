(library (asm-2 binary)
  (export
    put-binary
    u8-binary
    u16-binary
    bytevector-binary
    utf8-binary
    db-binary
    dw-binary
    binary-append
    binary->bytevector)
  (import (micascheme) (asm-2 u))

  (define-rules-syntax
    ((binary)
      (lambda ($port) (void)))
    ((binary ($port) body body* ...)
      (identifier? #'$port)
      (lambda ($port) body body* ...)))

  (define (put-binary $port $binary)
    ($binary $port))

  (define (u8-binary $u8)
    (binary ($port)
      (put-u8 $port $u8)))

  (define (u16-binary $u16 $endianness)
    (binary ($port)
      (put-u16 $port $u16 $endianness)))

  (define (bytevector-binary $bytevector)
    (binary ($port)
      (put-bytevector $port $bytevector)))

  (define (utf8-binary $string)
    (binary ($port)
      (put-bytevector $port (string->utf8 $string))))

  (define (db-binary $db $syntax)
    (binary ($port)
      (put-u8 $port (u8 $db $syntax))))

  (define (dw-binary $dw $syntax)
    (binary ($port)
      (put-u16 $port
        (u16 $dw $syntax)
        (endianness little))))

  (define (binary-append . $binaries)
    (binary ($port)
      (for-each
        (partial put-binary $port)
        $binaries)))

  (define (binary->bytevector $binary)
    (call-with-bytevector-output-port
      (lambda ($port)
        (put-binary $port $binary))))
)
