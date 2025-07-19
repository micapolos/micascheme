(library (asm-3 sized-located)
  (export sized-list->located-list)
  (import (micascheme) (asm-3 sized) (asm-3 located))

  (define (sized-list->located-list $sized-list)
    (reverse
      (sized-ref
        (fold-left
          (lambda ($sized-located-stack $sized)
            (sized
              (+
                (sized-size $sized-located-stack)
                (sized-size $sized))
              (push
                (sized-ref $sized-located-stack)
                (located (sized-size $sized-located-stack) (sized-ref $sized)))))
          (sized 0 (stack))
          $sized-list))))
)
