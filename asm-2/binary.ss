(library (asm-2 binary)
  (export
    db-binary
    dw-binary)
  (import (micascheme) (asm-2 u))

  (define (db-binary $db $syntax)
    (binary ($port)
      (put-u8 $port (u8 $db $syntax))))

  (define (dw-binary $dw $syntax)
    (binary ($port)
      (put-u16 $port
        (u16 $dw $syntax)
        (endianness little))))
)
