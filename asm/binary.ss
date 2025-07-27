(library (asm binary)
  (export db-binary dw-binary binary-endianness)
  (import (asm base))

  (define binary-endianness
    (make-thread-parameter (endianness little)))

  (define (db-binary . $dbs)
    (list->binary (map u8-binary $dbs)))

  (define (dw-binary . $dws)
    (list->binary (map (partial endianness-u16-binary (binary-endianness)) $dws)))
)
