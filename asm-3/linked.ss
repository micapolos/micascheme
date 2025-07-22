(library (asm-3 linked)
  (export
    linked
    linked?
    linked-start
    linked-bytevector
    linked->syntax
    linked->datum)
  (import (asm-3 base))

  (data (linked start bytevector))

  (define (linked->syntax $linked)
    #`(linked
      #,(literal->syntax (linked-start $linked))
      #,(bytevector->syntax (linked-bytevector $linked))))

  (define (linked->datum $linked)
    `(linked
      (start ,(linked-start $linked))
      (db ,@(bytevector->u8-list (linked-bytevector $linked)))))
)
