(library (asm)
  (export
    org eq u8
    asm asm?
    asm-org asm-with-org
    asm-labels asm-with-labels
    asm-values asm-with-values
    asm-blobs asm-with-blobs
    empty-asm
    asm+org
    asm+label
    asm+value
    asm+blob
    asm+syntax
    asm->syntax)
  (import (micascheme))

  (data (asm org labels values blobs))

  (define-aux-keywords org eq u8)

  (define (empty-asm)
    (asm 0 (stack) (stack) (stack)))

  (define (asm+label $asm $label)
    (asm-with-labels $asm
      (push (asm-labels $asm)
        (syntax-append $label (literal->syntax (asm-org $asm))))))

  (define (asm+org $asm $offset)
    (asm-with-org $asm
      (+ (asm-org $asm) $offset)))

  (define (asm+value $asm $id $value)
    (asm-with-values $asm
      (push (asm-values $asm)
        (syntax-append $id $value))))

  (define (asm+blob $asm $blob)
    (asm-with-blobs $asm
      (push (asm-blobs $asm) $blob)))

  (define (asm->syntax $asm)
    #`(lets
      #,@(reverse (asm-labels $asm))
      #,@(reverse (asm-values $asm))
      (blob->bytevector (blob-append #,@(reverse (asm-blobs $asm))))))

  (define (asm+syntax $asm $syntax)
    (syntax-case $syntax (eq org u8)
      (id
        (identifier? #'id)
        (asm+label $asm #'id))
      ((org expr)
        (asm-with-org $asm (datum expr)))
      ((eq id expr)
        (identifier? #'id)
        (asm+value $asm #'id #'expr))
      ((u8 expr ...)
        (fluent $asm
          (asm+blob #'(u8-blob expr ...))
          (asm+org (length (syntaxes expr ...)))))))
)
