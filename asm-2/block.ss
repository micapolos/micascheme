(library (asm-2 block)
  (export
    block block? block-size block-org->binary-proc
    block->binary
    list->block
    block-append)
  (import (micascheme) (syntax lookup))

  (data (block size org->binary-proc))

  (define (block->binary $block $org)
    ((block-org->binary-proc $block) $org))

  (define (list->block $blocks)
    (block
      (apply + (map block-size $blocks))
      (lambda ($org)
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
