(library (asm)
  (export
    asm asm?
    asm-org asm-with-org
    asm-labels asm-with-labels
    asm-values asm-with-values
    asm-blobs asm-with-blobs
    empty-asm
    asm+label
    asm->syntax)
  (import (micascheme))

  (data (asm org labels values blobs))

  (define (empty-asm)
    (asm 0 (stack) (stack) (stack)))

  (define (asm+label $asm $label)
    (asm-with-labels $asm
      (push (asm-labels $asm)
        (syntax-append $label (literal->syntax (asm-org $asm))))))

  (define (asm->syntax $asm)
    #`(lets
      #,@(reverse (asm-labels $asm))
      #,@(reverse (asm-values $asm))
      (blob->bytevector (blob-append #,@(reverse (asm-blobs $asm))))))
)
