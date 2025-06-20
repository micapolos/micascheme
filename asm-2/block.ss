(library (asm-2 block)
  (export
    block block?
    block-size block-labels block-puts
    block-with-size block-with-labels block-with-puts
    empty-block
    block-apply block-binary-syntax
    block+label block+data)
  (import (micascheme) (asm-2 u) (asm-2 binary) (syntax lookup))

  (data (block size labels puts))

  (define (empty-block)
    (block 0 (stack) (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+data $block $size $put)
    (block-with-puts
      (block-with-size $block (+ (block-size $block) $size))
      (push (block-puts $block) $put)))

  (define (block-apply $block $fn)
    ($fn $block))

  (define (block-binary-syntax $block $org)
    #`(let
      (#,@(map-with
        ($label (reverse (block-labels $block)))
        #`(#,(car $label) #,(datum->syntax #'+ (+ $org (cdr $label))))))
      (binary-append #,@(reverse (block-puts $block)))))
)
