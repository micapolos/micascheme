(library (asm-3 sized)
  (export
    sized sized? sized-size sized-ref
    sized-map
    sized+size
    sized->datum
    sized-update)
  (import (asm-3 base) (asm-3 size))

  (define-annotated (sized size))

  (define (sized+size $sized $size)
    (sized-with-size $sized
      (+ (sized-size $sized) $size)))

  (define (sized->datum $sized)
    `(sized
      ,(sized-size $sized)
      ,(sized-ref $sized)))

  (define (sized-update $sized $size-proc $ref-proc)
    (sized
      ($size-proc (sized-size $sized))
      ($ref-proc (sized-ref $sized))))
)
