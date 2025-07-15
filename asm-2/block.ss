(library (asm-2 block)
  (export
    block block? block-size block-org->binary-proc
    block->binary
    block-with
    list->block
    block-append
    block-with)
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

  (define-case-syntax (block-with line ...)
    (lets
      ($labels
        (filter-opts
          (map-with ($line #'(line ...))
            (syntax-case? $line ()
              (label
                (identifier? #'label)
                #'label)))))
      #`(lets
        #,@(map-with ($label $labels)
          #`(#,$label #f))
        (block-append
          #,@(map-with ($line #'(line ...))
            (syntax-case $line ()
              (label
                (identifier? #'label)
                #'(block 0
                  (lambda ($org)
                    (set! label $org)
                    (empty-binary))))
              (block
                #'block)))))))

)
