(library (asm binary)
  (export db-binary dw-binary binary-endianness)
  (import (asm base) (u))

  (define binary-endianness
    (make-thread-parameter (endianness little)))

  (define (db-binary . $dbs)
    (list->binary (map (dot u8-binary db->u8) $dbs)))

  (define (dw-binary . $dws)
    (list->binary (map (partial endianness-u16-binary (binary-endianness)) $dws)))

  (define (db->u8 $db)
    (switch $db
      ((s8? $s8) (bitwise-and #xff $s8))
      ((else $other) $other)))
)
