(library (asm-3 sized)
  (export
    sized sized? sized-size sized-ref
    sized+size
    sized-map
    list->sized
    sized-append)
  (import (micascheme))

  (data (sized size ref))

  (define (sized+size $sized $size)
    (sized-with-size $sized
      (+ (sized-size $sized) $size)))

  (define (sized-map $proc $sized)
    (sized-with-ref $sized
      ($proc (sized-ref $sized))))

  (define-list->/append (sized $sized-list)
    (sized
      (apply + (map sized-size $sized-list))
      (map sized-ref $sized-list)))
)
