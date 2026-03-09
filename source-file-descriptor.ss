(library (source-file-descriptor)
  (export
    source-file-descriptor=?
    path->source-file-descriptor)
  (import (chezscheme))

  (define (source-file-descriptor=? $a $b)
    (and
      (equal?
        (source-file-descriptor-path $a)
        (source-file-descriptor-path $b))
      (=
        (source-file-descriptor-checksum $a)
        (source-file-descriptor-checksum $b))))

  (define (path->source-file-descriptor $path)
    (call-with-port
      (open-file-input-port $path)
      (lambda ($port)
        (make-source-file-descriptor $path $port))))
)
