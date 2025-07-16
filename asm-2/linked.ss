(library (asm-2 linked)
  (export linked linked? linked-start linked-ref)
  (import (micascheme) (asm-2 linker))

  (data (linked start ref))

  (define (linked-bytevector $lookup $identifier $org)
    (lets
      ($linker (identifier-linker $lookup $identifier))
      (linked
        (linker-ref $linker $identifier)
        (linker-bytevector $linker))))

)
