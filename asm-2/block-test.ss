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

(lets
  ($block
    (block-with
      line-1
      (block 1 (lambda (_) (u8-binary 10)))
      line-2
      (block 2 (lambda (_) (binary-append (u8-binary 20) (u8-binary 30))))
      line-3
      (block 3 (lambda (_) (binary-append (u8-binary line-1) (u8-binary line-2) (u8-binary line-3))))))
  (run
    (check (= (block-size $block) 6))
    (check
      (equal?
        (binary->bytevector (block->binary $block 100))
        (bytevector 10 20 30 100 101 103)))))
