(library (asm-3 binary)
  (export db-binary dw-binary)
  (import (asm-3 base))

  (define (db-binary . $dbs)
    (list->binary (map u8-binary $dbs)))

  (define (dw-binary . $dws)
    (list->binary (map (partial endianness-u16-binary (endianness little)) $dws)))
)
