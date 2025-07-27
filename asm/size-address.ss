(library (asm size-address)
  (export sizes->addresses)
  (import (asm base) (asm sized) (asm located))

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
