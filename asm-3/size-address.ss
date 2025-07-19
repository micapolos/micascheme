(library (asm-3 size-address)
  (export sizes->addresses)
  (import (micascheme) (asm-3 sized) (asm-3 located))

  (define (sizes->addresses $sizes)
    (reverse
      (sized-ref
        (fold-left
          (lambda ($sized-addresses $size)
            (sized
              (+ (sized-size $sized-addresses) $size)
              (push (sized-ref $sized-addresses) (sized-size $sized-addresses))))
          (sized 0 (stack))
          $sizes))))
)
