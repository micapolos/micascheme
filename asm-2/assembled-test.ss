(import (micascheme) (asm-2 assembled) (asm-2 block) (asm-2 fragment) (syntax lookup))

(check
  (equal?
    (fragment->bytevector
      (lookup-with
        (foo
          (fragment-with
            (block 2
              (lambda ($org)
                (binary-append
                  (u8-binary $org)
                  (u8-binary 10))))))
        (bar
          (fragment-with (dep foo)
            (block 2
              (lambda ($org)
                (binary-append
                  (u8-binary $org)
                  (u8-binary 20)))))))
      #'bar
      100)
    (bytevector 100 20 102 10)))
