(library (asm-3 block)
  (export
    block? block-alignment block-size block-deps

    empty-block
    block+dep
    block+label
    block+define
    block+binary
    block+zeros
    block-align
    block->syntax
    block->datum
    check-block)
  (import (micascheme) (syntax lookup))

  (define-keywords $org $port)

  (data (block-transformer proc))
  (data (block alignment size deps label-syntaxes define-syntaxes run-syntaxes))

  (define (empty-block)
    (block 1 0 (stack) (stack) (stack) (stack)))

  (define (block+dep $block $dep)
    (lets
      ($deps (block-deps $block))
      (block-with-deps $block
        (cond
          ((memp (partial free-identifier=? $dep) $deps) $deps)
          (else (push $deps $dep))))))

  (define (block+label $block $label)
    (block-with-label-syntaxes $block
      (push
        (block-label-syntaxes $block)
        #`(#,$label (+ org #,(literal->syntax (block-size $block)))))))

  (define (block+define $block $identifier $syntax)
    (block-with-define-syntaxes $block
      (push
        (block-define-syntaxes $block)
        #`(#,$identifier #,$syntax))))

  (define (block+binary $block $size $syntax)
    (fluent $block
      (block-with-run-syntaxes
        (push
          (block-run-syntaxes $block)
          #`(put-binary $port #,$syntax)))
      (block-with-size
        (+ (block-size $block) $size))))

  (define (block+zeros $block $size)
    (if (zero? $size)
      $block
      (block+binary $block $size
        #`(zero-binary #,(literal->syntax $size)))))

  (define (block-align $block $alignment)
    (lets
      ($size (block-size $block))
      (fluent $block
        (block-with-alignment (max (block-alignment $block) $alignment))
        (block+zeros (- (bitwise-align $size $alignment) $size)))))

  (define (block->syntax $block)
    #`(lambda ($org)
      (lambda ($port)
        (lets
          #,@(reverse (block-label-syntaxes $block))
          #,@(reverse (block-define-syntaxes $block))
          (run #,@(reverse (block-run-syntaxes $block)))))))

  (define (block->datum $block)
    `(block
      (alignment ,(block-alignment $block))
      (size ,(block-size $block))
      (deps ,@(reverse (map syntax->datum (block-deps $block))))
      ,@(reverse (map syntax->datum (block-label-syntaxes $block)))
      ,@(reverse (map syntax->datum (block-define-syntaxes $block)))
      ,@(reverse (map syntax->datum (block-run-syntaxes $block)))))

  (define-rule-syntax (check-block in out)
    (check (equal? (block->datum in) 'out)))
)
