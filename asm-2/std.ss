(library (asm-2 std)
  (export + string-append string-length)
  (import (asm-2 lang) (prefix (micascheme) %))

  (define + (typed (procedure integer integer) %+))
  (define string-append (typed (procedure string string) %string-append))
  (define string-length (typed (procedure (string) integer) %string-length))
)
