(library (blob)
  (export
    blob blob? blob-put-proc blob-size
    empty-blob
    u8-blob
    put-blob
    list->blob
    blob-append
    bytevector->blob
    utf8->blob
    blob->bytevector
    blob->syntax)
  (import (scheme) (data) (lets) (procedure) (fluent) (syntax) (port))

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

  (define (utf8->blob $string)
    (bytevector->blob (string->utf8 $string)))

  (define (put-blob $port $blob)
    ((blob-put-proc $blob) $port))

  (define (list->blob $blobs)
    (blob
      (apply + (map blob-size $blobs))
      (lambda ($port) (for-each (partial put-blob $port) $blobs))))

  (define (blob-append . $blobs)
    (list->blob $blobs))

  (define (blob->bytevector $blob)
    (with-bytevector-output-port $port
      (put-blob $port $blob)))

  (define (blob->syntax $blob)
    (fluent $blob
      (blob->bytevector)
      (bytevector->syntax)
      (let $it #`(bytevector->blob #,$it))))
)
