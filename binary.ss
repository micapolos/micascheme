(library (binary)
  (export
    binary
    empty-binary
    put-binary
    list->binary
    binary-append
    binary->bytevector
    fill-binary
    zero-binary
    u8-binary
    u16-binary
    bytevector-binary
    utf8-binary
    binary->datum)
  (import (scheme) (syntax) (syntaxes) (procedure) (port))

  (define-rules-syntax
    ((binary)
      (binary ($port) (void)))
    ((binary ($port) body body* ...)
      (identifier? #'$port)
      (lambda ($port) body body* ...)))

  (define (empty-binary)
    (lambda ($port) (void)))

  (define (put-binary $port $binary)
    ($binary $port))

  (define (binary->bytevector $binary)
    (call-with-bytevector-output-port
      (lambda ($port)
        (put-binary $port $binary))))

  (define (list->binary $binaries)
    (binary ($port)
      (for-each
        (partial put-binary $port)
        $binaries)))

  (define (binary-append . $binaries)
    (list->binary $binaries))

  (define (fill-binary $size $u8)
    (binary ($port)
      (repeat $size
        (put-u8 $port $u8))))

  (define (zero-binary $size)
    (fill-binary $size 0))

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

  (define (binary->datum $binary)
    `(binary ,@(bytevector->u8-list (binary->bytevector $binary))))
)
