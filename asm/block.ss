(library (asm block)
  (export
    block block?
    block-org block-labels block-binary-syntaxes
    block-with-org block-with-labels block-with-binary-syntaxes
    empty-block
    block-binary-syntax
    block+label block+binary-syntax block-bind
    block+zeroes)
  (import
    (micascheme)
    (asm binary))

  (data (block org labels binary-syntaxes))

  (define (empty-block $org)
    (block $org (stack) (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-org $block)))))

  (define (block+binary-syntax $block $size $binary-syntax)
    (block-with-binary-syntaxes
      (block-with-org $block (+ (block-org $block) $size))
      (push (block-binary-syntaxes $block) $binary-syntax)))

  (define (block-bind $block $proc)
    (lets
      ($local-block ($proc (empty-block (block-org $block))))
      (block
        (block-org $local-block)
        (block-labels $block)
        (push
          (block-binary-syntaxes $block)
          (block-binary-syntax $local-block)))))

  (define (block+zeroes $block $size)
    (block+binary-syntax $block $size
      #`(zero-binary #,(literal->syntax $size))))

  (define (block-align $block $alignment)
    (block+zeroes $block
      (bitwise-align (block-org $block) $alignment)))

  (define (block-binary-syntax $block)
    #`(let
      (#,@(map-with
        ($label (reverse (block-labels $block)))
        #`(#,(car $label) #,(cdr $label))))
      #,(or
        (single (block-binary-syntaxes $block))
        #`(binary-append #,@(reverse (block-binary-syntaxes $block))))))
)
