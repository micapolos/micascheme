(library (asm-3 sized-relocable)
  (export
    sized-relocable-append
    list->sized-relocable
    check-sized-relocable
    offset-sized-list)
  (import (asm-3 base) (asm-3 size) (asm-3 sized) (asm-3 relocable))

  (define-list->/append (sized-relocable $sized-relocables)
    (map-sized reverse
      (fold-left
        (lambda ($sized-relocable-stack $sized-relocable-item)
          (lets
            ($stack-size (sized-size $sized-relocable-stack))
            ($item-size (sized-size $sized-relocable-item))
            ($relocable-stack (sized-ref $sized-relocable-stack))
            ($relocable-item (sized-ref $sized-relocable-item))
            (sized
              (size+ $stack-size $item-size)
              (push $relocable-stack (relocable+offset $relocable-item $stack-size)))))
        (pure-sized (stack))
        $sized-relocables)))

  (define (offset-sized-list $offset-proc $sized-list)
    (map-sized reverse
      (fold-left
        (lambda ($sized-stack $sized-item)
          (lets
            ($stack-size (sized-size $sized-stack))
            ($item-size (sized-size $sized-item))
            ($stack (sized-ref $sized-stack))
            ($item (sized-ref $sized-item))
            (sized
              (size+ $stack-size $item-size)
              (push $stack ($offset-proc $stack-size $item)))))
        (pure-sized (stack))
        $sized-list)))

  (define-rule-syntax (check-sized-relocable org sized-relocable out)
    (check
      (equal?
        (sized-map sized-relocable
          (partial-flip relocable-ref org))
        out)))
)
