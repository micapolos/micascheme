(library (blob)
  (export
    blob blob? blob-put-proc blob-size
    empty-blob
    u8-blob
    put-blob
    list->blob
    blob-append
    bytevector->blob
    blob->bytevector)
  (import (scheme) (data) (lets) (procedure))

  (data (blob size put-proc))

  (define (empty-blob)
    (blob 0 (lambda ($port) (void))))

  (define (u8-blob . $u8s)
    (blob
      (length $u8s)
      (lambda ($port)
        (for-each (partial put-u8 $port) $u8s))))

  (define (bytevector->blob $bytevector)
    (blob
      (bytevector-length $bytevector)
      (lambda ($port)
        (put-bytevector $port $bytevector))))

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
