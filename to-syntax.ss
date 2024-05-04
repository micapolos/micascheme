(library (to-syntax)
  (export
    bytevector->syntax
    bytevector->immutable-syntax
    vector->syntax
    vector->immutable-syntax)
  (import (micascheme))

  (define (bytevector->syntax $bytevector)
    #`(bytevector
      #,@(map (partial datum->syntax #'+)
        (bytevector->u8-list $bytevector))))

  (define (bytevector->immutable-syntax $bytevector)
    #`(bytevector->immutable-bytevector
      #,(bytevector->syntax $bytevector)))

  (define (vector->syntax $vector $item-fn)
    #`(vector
      #,@(map $item-fn (vector->list $vector))))

  (define (vector->immutable-syntax $vector $item-fn)
    #`(immutable-vector
      #,@(map $item-fn (vector->list $vector))))
)
