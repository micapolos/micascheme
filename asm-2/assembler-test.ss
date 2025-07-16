(import (micascheme) (asm-2 assembler) (asm-2 block) (asm-2 fragment) (syntax lookup))

(check
  (equal?
    (assembler-bytevector
      (identifier-assembler
        (lookup-with
          (foo
            (fragment-with
              (block-with 2 ($org)
                (binary-append
                  (u8-binary $org)
                  (u8-binary 10)))))
          (bar
            (fragment-with (dep foo)
              (block-with 2 ($org)
                (binary-append
                  (u8-binary $org)
                  (u8-binary 20))))))
        #'bar
        100))
    (bytevector 100 10 102 20)))
