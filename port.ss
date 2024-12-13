(library (port)
  (export put-u16 with-bytevector-output-port)
  (import (scheme) (bytevector) (syntax))

  (define (put-u16 $port $u16 $endianness)
    (put-bytevector $port (u16-bytevector $u16 $endianness)))

  (define-rule-syntax (with-bytevector-output-port port body ...)
    (call-with-bytevector-output-port
      (lambda (port) body ...)))
)
