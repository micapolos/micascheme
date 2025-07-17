(library (asm-3 sized-relocable)
  (export
    list->sized-relocable
    sized-relocable-append)
  (import (micascheme) (asm-3 sized) (asm-2 relocable))

  (define-list->/append (sized-relocable $sized-relocables)
    (sized-map
      (lambda ($relocable-stack)
        (list->relocable (reverse $relocable-stack)))
      (fold-left
        (lambda ($sized-relocable-stack $sized-relocable)
          (sized
            (+
              (sized-size $sized-relocable-stack)
              (sized-size $sized-relocable))
            (push
              (sized-ref $sized-relocable-stack)
              (relocable+offset
                (sized-ref $sized-relocable)
                (sized-size $sized-relocable-stack)))))
        (sized 0 (stack))
        $sized-relocables)))
)
