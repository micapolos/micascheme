(import (micascheme) (asm-2 assembler))

(check
  (equal?
    (assemble
      (pure-assembler 10))
    10))

(check
  (equal?
    (assemble
      (assembler-bind-with
        (_ (define-assembler #'a "a"))
        (_ (define-assembler #'b "b"))
        ($a (ref-assembler #'a))
        ($b (ref-assembler #'b))
        (pure-assembler (string-append $a $b))))
    "ab"))

(check
  (equal?
    (filter string?
      (assemble
        (assembler-append
          (define-assembler #'a "a")
          (define-assembler #'b "b")
          (ref-assembler #'a)
          (ref-assembler #'b)
          (assembler-bind-with
            ($a (ref-assembler #'a))
            ($b (ref-assembler #'b))
            (pure-assembler (string-append $a $b))))))
    '("a" "b" "ab")))
