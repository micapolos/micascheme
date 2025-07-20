(library (asm-2 aligned-sized)
  (export aligned-sized-append)
  (import (asm-3 base) (asm-2 aligned) (asm-3 sized) (asm-3 size) (asm-2 alignment))

  (define (map-aligned-sized $proc $aligned-sized)
    (aligned-map $aligned-sized
      (lambda ($sized)
        (sized-map $sized $proc))))

  (define (aligned-sized-append $slack-proc $ref-append . $aligned-sized-list)
    (map-aligned-sized (dot (partial apply $ref-append) reverse)
      (fold-left
        (lambda ($aligned-sized-stack $aligned-sized)
          (lets
            ($stack-alignment (aligned-alignment $aligned-sized-stack))
            ($item-alignment (aligned-alignment $aligned-sized))
            ($alignment (alignment-append $stack-alignment $item-alignment))
            ($sized-stack (aligned-ref $aligned-sized-stack))
            ($sized-item (aligned-ref $aligned-sized))
            ($stack-size (sized-size $sized-stack))
            ($item-size (sized-size $sized-item))
            ($slack-size (- (bitwise-align $stack-size $alignment) $stack-size))
            ($size (size+ $stack-size $slack-size $item-size))
            ($stack (sized-ref $sized-stack))
            ($item (sized-ref $sized-item))
            (aligned $alignment
              (sized $size
                (if (zero? $slack-size)
                  (push $stack $item)
                  (push (push $stack ($slack-proc $slack-size)) $item))))))
        (pure-aligned (pure-sized (stack)))
        $aligned-sized-list)))
)
