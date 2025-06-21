(library (asm block)
  (export
    block block?
    block-size block-labels block-binary-syntax-procs
    block-with-size block-with-labels block-with-binary-syntax-procs
    empty-block
    block-apply block-binary-syntax
    block+label block+binary-syntax-proc block+local)
  (import (micascheme) (asm binary))

  (data (block size labels binary-syntax-procs))

  (define (empty-block)
    (block 0 (stack) (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+binary-syntax-proc $block $size $binary-syntax-proc)
    (block-with-binary-syntax-procs
      (block-with-size $block (+ (block-size $block) $size))
      (push (block-binary-syntax-procs $block) $binary-syntax-proc)))

  (define (block+local $block $local-block)
    (block+binary-syntax-proc
      $block
      (block-size $local-block)
      (lambda ($org)
        (block-binary-syntax $local-block
          (+ $org (block-size $block))))))

  (define (block-apply $block $fn)
    ($fn $block))

  (define (block-binary-syntax $block $org)
    #`(let
      (#,@(map-with
        ($label (reverse (block-labels $block)))
        #`(#,(car $label) #,(datum->syntax #'+ (+ $org (cdr $label))))))
      (binary-append
        #,@(map
          (lambda ($binary-syntax-proc) ($binary-syntax-proc $org))
          (reverse (block-binary-syntax-procs $block))))))
)
