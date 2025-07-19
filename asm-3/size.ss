(library (asm-3 size)
  (export empty-size size+)
  (import (asm-3 base))

  (define (empty-size) 0)
  (define (size+ . $sizes) (apply + $sizes))
)
