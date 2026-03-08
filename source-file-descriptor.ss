(library (source-file-descriptor)
  (export source-file-descriptor=?)
  (import (chezscheme))

  (define (source-file-descriptor=? $a $b)
    (and
      (equal?
        (source-file-descriptor-path $a)
        (source-file-descriptor-path $b))
      (=
        (source-file-descriptor-checksum $a)
        (source-file-descriptor-checksum $b))))
)
