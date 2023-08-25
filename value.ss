(library (value)
  (export 
    value-tuple
    value-index
    value-indexed)
  (import (micascheme))

  (define (value-tuple $size $value)
    (case $size
      ((0) (list))
      ((1) (list $value))
      ((2) (list (car $value) (cdr $value)))
      (else (vector->list $value))))

  (define (value-index $size $value)
    (case $size
      ((1) 0)
      ((2) (if $value 0 1))
      (else $value)))

  (define (value-indexed $size $value)
    (case $size
      ((1) (cons 0 $value))
      (else (cons (value-index $size (car $value)) (cdr $value)))))
)
