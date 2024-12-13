(library (port)
  (export put-u16)
  (import (scheme) (bytevector))

  (define (put-u16 $port $u16 $endianness)
    (put-bytevector $port (u16-bytevector $u16 $endianness)))
)
