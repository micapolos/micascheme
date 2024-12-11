(library (z80 blob)
  (export
    blob blob? blob-put-proc blob-size
    u8-blob
    put-blob
    list->blob
    blob-append
    blob->bytevector)
  (import (micascheme))

  (data (blob size put-proc))

  (define (u8-blob $u8)
    (blob 1 (lambda ($port) (put-u8 $port $u8))))

  (define (put-blob $port $blob)
    ((blob-put-proc $blob) $port))

  (define (list->blob $blobs)
    (blob
      (apply + (map blob-size $blobs))
      (lambda ($port) (for-each (partial put-blob $port) $blobs))))

  (define (blob-append . $blobs)
    (list->blob $blobs))

  (define (blob->bytevector $blob)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      (run (put-blob $port $blob))
      ($close)))
)
