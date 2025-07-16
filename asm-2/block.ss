(library (asm-2 block)
  (export
    block block? block-size block-binary-expression
    block-with
    block->binary
    list->block
    block-append)
  (import (micascheme) (asm-2 expression) (syntax lookup))

  (data (block size binary-expression))

  (define-rules-syntax
    ((block-with size body)
      (block size (expression-with body)))
    ((block-with size ($org) body)
      (block size (expression-with ($org) body))))

  (define (block->binary $block $org)
    (expression->value (block-binary-expression $block) $org))

  (define (list->block $blocks)
    (block
      (apply + (map block-size $blocks))
      (expression-with ($org)
        (lets
          ((pair $org $binaries)
            (fold-left
              (lambda ($org/binaries $block)
                (lets
                  ((pair $org $binaries) $org/binaries)
                  (cons
                    (+ $org (block-size $block))
                    (push $binaries (block->binary $block $org)))))
              (cons $org (stack))
              $blocks))
          (list->binary (reverse $binaries))))))

  (define (block-append . $blocks)
    (list->block $blocks))
)
