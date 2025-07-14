(import (micascheme) (asm-2 assembled) (asm-2 block) (asm-2 fragment) (syntax lookup))

(check
  (equal?
    (fragment->bytevector
      (lookup-with
        (foo
          (fragment-with
            (block ($org)
              (u8-blob $org 10))))
        (bar
          (fragment-with (dep foo)
            (block ($org) (u8-blob $org 20)))))
      #'bar
      100)
    (bytevector 100 20 102 10)))
