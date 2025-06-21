(library (asm block)
  (export
    block block?
    block-size block-labels block-binary-syntaxes
    block-with-size block-with-labels block-with-binary-syntaxes
    empty-block
    block-apply block-binary-syntax
    block+label block+binary-syntax block+local)
  (import (micascheme) (asm binary))

  (data (block size labels binary-syntaxes))

  (define (empty-block)
    (block 0 (stack) (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+binary-syntax $block $size $binary-syntax)
    (block-with-binary-syntaxes
      (block-with-size $block (+ (block-size $block) $size))
      (push (block-binary-syntaxes $block) $binary-syntax)))

  (define (block+local $block $local-block)
    (block+binary-syntax
      $block
      (block-size $local-block)
      (block-binary-syntax $local-block (block-size $block))))

  (define (block-apply $block $fn)
    ($fn $block))

  (define (block-binary-syntax $block $org)
    #`(let
      (#,@(map-with
        ($label (reverse (block-labels $block)))
        #`(#,(car $label) #,(datum->syntax #'+ (+ $org (cdr $label))))))
      (binary-append #,@(reverse (block-binary-syntaxes $block)))))
)
