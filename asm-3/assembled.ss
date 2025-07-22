(library (asm-3 assembled)
  (export
    assembled
    assembled?
    assembled-start
    assembled-bytevector
    assembled->syntax
    assembled->datum)
  (import (asm-3 base))

  (data (assembled start bytevector))

  (define (assembled->syntax $assembled)
    #`(assembled
      #,(literal->syntax (assembled-start $assembled))
      #,(bytevector->syntax (assembled-bytevector $assembled))))

  (define (assembled->datum $assembled)
    `(assembled
      (start ,(assembled-start $assembled))
      (db ,@(bytevector->u8-list (assembled-bytevector $assembled)))))
)
