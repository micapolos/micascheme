(library (z80 asm-blob)
  (export
    asm asm?
    asm-org asm-with-org
    asm-labels asm-with-labels
    asm-values asm-with-values
    asm-blobs asm-with-blobs
    empty-asm
    asm->syntax)
  (import (micascheme))

  (data (asm org labels values blobs))

  (define (empty-asm)
    (asm 0 (stack) (stack) (stack)))

  (define (asm->syntax $asm)
    #`(lets
      #,@(reverse (asm-labels $asm))
      #,@(reverse (asm-values $asm))
      (blob->bytevector (blob-append #,@(reverse (asm-blobs $asm))))))
)
