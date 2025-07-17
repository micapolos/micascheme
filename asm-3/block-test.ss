(import (micascheme) (asm-3 block))

(check-block
  (fluent (empty-block))
  (block
    (alignment 1)
    (size 0)
    (labels)
    (defs)
    (body)))

(check-block
  (fluent (empty-block)
    (block+define #'size #'(- end start))
    (block+label #'start)
    (block+binary 1 #`binary-1)
    (block+binary 2 #`binary-2)
    (block-align 8)
    (block+label #'mid)
    (block+binary 3 #`binary-3)
    (block+label #'end))
  (block
    (alignment 8)
    (size 11)
    (labels
      (start (+ org 0))
      (mid (+ org 8))
      (end (+ org 11)))
    (defs
      (size (- end start)))
    (body
      (put-binary $port binary-1)
      (put-binary $port binary-2)
      (put-binary $port (zero-binary 5))
      (put-binary $port binary-3))))
