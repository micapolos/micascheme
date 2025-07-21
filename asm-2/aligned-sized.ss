(library (asm-2 aligned-sized)
  (export
    map-aligned-sized
    aligned-sized-map
    aligned-sized-append
    list->aligned-sized)
  (import
    (asm-3 base)
    (asm-2 aligned)
    (asm-3 sized)
    (asm-3 size)
    (asm-2 alignment))

  (define (map-aligned-sized $proc $aligned-sized)
    (aligned-map $aligned-sized
      (lambda ($sized)
        (sized-map $sized $proc))))

  (define (aligned-sized-map $aligned-sized $proc)
    (map-aligned-sized $proc $aligned-sized))

  (define (aligned-sized-append $slack-proc . $aligned-sized-list)
    (map-aligned (dot reverse sized-ref)
      (fold-left
        (lambda ($aligned-sized-sized-stack $aligned-sized)
          (lets
            ($stack-alignment (aligned-alignment $aligned-sized-sized-stack))
            ($item-alignment (aligned-alignment $aligned-sized))
            ($alignment (alignment-append $stack-alignment $item-alignment))
            ($sized-sized-stack (aligned-ref $aligned-sized-sized-stack))
            ($sized-item (aligned-ref $aligned-sized))
            ($stack-size (sized-size $sized-sized-stack))
            ($item-size (sized-size $sized-item))
            ($slack-size (- (bitwise-align $stack-size $alignment) $stack-size))
            ($size (size+ $stack-size $slack-size $item-size))
            ($sized-stack (sized-ref $sized-sized-stack))
            (aligned $alignment
              (sized $size
                (push
                  (if (zero? $slack-size)
                    $sized-stack
                    (push $sized-stack (sized $slack-size ($slack-proc $slack-size))))
                  $sized-item)))))
        (pure-aligned (pure-sized (stack)))
        $aligned-sized-list)))

  (define (list->aligned-sized $aligned-sized-list $slack-proc)
    (apply aligned-sized-append $slack-proc $aligned-sized-list))
)
