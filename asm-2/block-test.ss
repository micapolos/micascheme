(import (micascheme) (asm-2 block))

(lets
  ($block
    (block-append
      (block 1 (lambda (_) (u8-binary 10)))
      (block 2 (lambda (_) (binary-append (u8-binary 20) (u8-binary 30))))
      (block 1 (lambda ($org) (u8-binary $org)))))
  (run
    (check (= (block-size $block) 4))
    (check
      (equal?
        (binary->bytevector (block->binary $block 100))
        (bytevector 10 20 30 103)))))
