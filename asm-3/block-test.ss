(import (micascheme) (asm-3 block) (asm-2 relocable))

(check-block
  (fluent (empty-block))
  100
  (block
    (alignment 1)
    (size 0)
    (lets (binary-append))))

(check-block
  (fluent (empty-block)
    (block+zeros 10))
  100
  (block
    (alignment 1)
    (size 10)
    (lets
      (binary-append
        (zero-binary 10)))))

(check-block
  (fluent (empty-block)
    (block+define #'size #'(- end start))
    (block+label #'start)
    (block+binary 1 (relocable-with #`binary-1))
    (block+binary 2 (relocable-with #`binary-2))
    (block-align 8)
    (block+label #'mid)
    (block+binary 3 (relocable-with #`binary-3))
    (block+label #'end))
  100
  (block
    (alignment 8)
    (size 11)
    (lets
      (start 100)
      (mid 108)
      (end 111)
      (size (- end start))
      (binary-append
        binary-1
        binary-2
        (zero-binary 5)
        binary-3))))
