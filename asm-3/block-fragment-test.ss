(import (asm-3 base) (asm-3 block) (asm-3 fragment) (asm-3 expression) (asm-3 block-fragment))

(check-fragment
  (block->fragment (empty-block))
  (dependent
    (aligned 1
      (sized 0
        (relocable-with ($org)
          (let ()
            (binary-append)))))))

(check-fragment
  (block->fragment
    (block-append
      (identifier-block #'start)
      (dw-block
        (identifier-expression #'start)
        (identifier-expression #'end))
      (db-block
        (identifier-expression #'foo)
        (identifier-expression #'bar))
      (identifier-block #'end)))
  (dependent (foo bar)
    (aligned 1
      (sized 6
        (relocable-with ($org)
          (let
            (
              (start (+ $org 0))
              (end (+ $org 6)))
            (binary-append
              (dw-binary start end)
              (db-binary foo bar))))))))
