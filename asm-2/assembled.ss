(library (asm-2 assembled)
  (export assembled assembled? assembled-start assembled-ref)
  (import (micascheme) (asm-2 assembler))

  (data (assembled start ref))

  (define (assembled-bytevector $lookup $identifier $org)
    (lets
      ($assembler (identifier-assembler $lookup $identifier))
      (assembled
        (assembler-ref $assembler $identifier)
        (assembler-bytevector $assembler))))

)
