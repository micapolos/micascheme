(library (asm size)
  (export empty-size size+)
  (import (asm base))

  (define (empty-size) 0)
  (define (size+ . $sizes) (apply + $sizes))
)
