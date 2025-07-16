(import (micascheme) (asm-2 block))

(lets
  ($block
    (block-append
      (block-with 1 (u8-binary 10))
      (block-with 2 (binary-append (u8-binary 20) (u8-binary 30)))
      (block-with 1 ($org) (u8-binary $org))))
  (run
    (check (= (block-size $block) 4))
    (check
      (equal?
        (binary->bytevector (block->binary $block 100))
        (bytevector 10 20 30 103)))))
