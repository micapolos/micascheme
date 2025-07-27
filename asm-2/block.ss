(library (asm-2 block)
  (export
    block block? block-size block-relocable-binary
    block-with
    block->binary
    list->block
    block-append)
  (import (micascheme) (asm-3 relocable) (syntax lookup))

  (data (block size relocable-binary))

  (define-rules-syntax
    ((block-with size body)
      (block size (relocable-with body)))
    ((block-with size ($org) body)
      (block size (relocable-with ($org) body))))

  (define (block->binary $block $org)
    (relocable-ref (block-relocable-binary $block) $org))

  (define (list->block $blocks)
    (block
      (apply + (map block-size $blocks))
      (relocable-with ($org)
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
