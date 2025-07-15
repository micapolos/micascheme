(import (micascheme) (syntax lookup) (asm-2 asm) (asm-2 fragment) (asm-2 block))

(lets
  ($fragment
    (asm
      line-1
      (fragment-with (dep a)
        (block 1
          (lambda (_)
            (u8-binary (dep a)))))
      line-2
      (fragment-with (dep b c)
        (block 2
          (lambda (_)
            (binary-append
              (u8-binary (dep b))
              (u8-binary (dep c))))))
      line-3
      (fragment-with
        (block 3
          (lambda (_)
            (binary-append
              (u8-binary line-1)
              (u8-binary line-2)
              (u8-binary line-3)))))))
  ($block
    (fragment-ref $fragment
      (lookup-with (a 10) (b 20) (c 30))))
  (run
    (check (= (block-size $block) 6))
    (check
      (equal?
        (binary->bytevector (block->binary $block 100))
        (bytevector 10 20 30 100 101 103)))))
