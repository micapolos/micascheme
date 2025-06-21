(library (asm block)
  (export
    block block?
    block-size block-labels block-binary-syntax-procs
    block-with-size block-with-labels block-with-binary-syntax-procs
    empty-block
    block-binary-syntax
    block+label block+binary-syntax-proc block+local
    block+zeroes)
  (import
    (micascheme)
    (asm binary))

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
          (+ (block-size $block) $org)))))

  (define (block+zeroes $block $size)
    (block+binary-syntax-proc $block $size
      (lambda ($org)
        #`(zero-binary #,(literal->syntax $size)))))

  (define (block-align $block $alignment)
    (block+zeroes $block
      (bitwise-align (block-size $block) $alignment)))

  (define (block-binary-syntax $block $org)
    #`(let
      (#,@(map-with
        ($label (reverse (block-labels $block)))
        #`(#,(car $label) #,(literal->syntax (+ $org (cdr $label))))))
      (binary-append
        #,@(map
          (lambda ($binary-syntax-proc) ($binary-syntax-proc $org))
          (reverse (block-binary-syntax-procs $block))))))
)
