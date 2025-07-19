(library (asm-3 sized)
  (export
    sized sized? sized-size sized-ref
    sized+size
    sized-map
    list->sized
    sized-append
    sized->datum)
  (import (asm-3 base) (asm-3 size))

  (define-scoped (sized size))

  (define (sized+size $sized $size)
    (sized-with-size $sized
      (+ (sized-size $sized) $size)))

  (define (sized->datum $ref->datum $sized)
    `(sized
      ,(sized-size $sized)
      ,($ref->datum (sized-ref $sized))))
)
