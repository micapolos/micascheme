(library (asm-3 sized-relocable)
  (export
    sized-relocable-append
    list->sized-relocable
    check-sized-relocable)
  (import (asm-3 base) (asm-3 size) (asm-3 sized) (asm-2 relocable))

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

  (define-rule-syntax (check-sized-relocable org sized-relocable out)
    (check
      (equal?
        (sized-map sized-relocable
          (partial-flip relocable-ref org))
        out)))
)
