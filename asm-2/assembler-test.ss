(import (micascheme) (asm-2 assembler) (asm-2 block) (asm-2 fragment) (syntax lookup))

(check
  (equal?
    (assembler-ref
      (identifier-assembler
        (lookup-with (value 10))
        #'value
        100)
      #'value)
    10))

(check
  (equal?
    (assembler-ref
      (identifier-assembler
        (lookup-with
          (value (fragment-with 10)))
        #'value
        100)
      #'value)
    10))

(check
  (equal?
    (assembler-ref
      (identifier-assembler
        (lookup-with
          (value-10 (fragment-with 10))
          (value-11
            (fragment-with (dep value-10)
              (+ (dep value-10) 1))))
        #'value-11
        100)
      #'value-11)
    11))

(check
  (equal?
    (assembler-bytevector
      (identifier-assembler
        (lookup-with
          (value-10
            (fragment-with 10))
          (value-20
            (fragment-with (dep value-10)
              (+ (dep value-10) (dep value-10))))
          (foo
            (fragment-with (dep value-10)
              (block-with 2 ($org)
                (binary-append
                  (u8-binary $org)
                  (u8-binary (dep value-10))))))
          (bar
            (fragment-with (dep foo value-20)
              (block-with 2 ($org)
                (binary-append
                  (u8-binary $org)
                  (u8-binary (dep value-20)))))))
        #'bar
        100))
    (bytevector 100 10 102 20)))
