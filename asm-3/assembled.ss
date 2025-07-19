(library (asm-3 assembled)
  (export
    assembled
    assembled?
    assembled-start-address
    assembled-bytevector
    assembled->syntax)
  (import (asm-3 base))

  (data (assembled start-address bytevector))

  (define (assembled->syntax $assembled)
    #`(assembled
      #,(literal->syntax (assembled-start-address $assembled))
      #,(bytevector->syntax (assembled-bytevector $assembled))))
)
