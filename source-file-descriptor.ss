(library (source-file-descriptor)
  (export
    source-file-descriptor-extension
    source-file-descriptor=?
    path->source-file-descriptor
    load-datum/annotations)
  (import (chezscheme) (lets))

  (define (source-file-descriptor-extension $sfd)
    (path-extension (source-file-descriptor-path $sfd)))

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

  (define (load-datum/annotations $path)
    (lets
      ($sfd (path->source-file-descriptor $path))
      (get-datum/annotations (open-source-file $sfd) $sfd 0)))
)
