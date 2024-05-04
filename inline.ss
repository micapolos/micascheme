(library (inline)
  (export
    inline-bytevector)
  (import (scheme) (syntax) (procedure))

  (define-rule-syntax (inline-bytevector $expr)
    (syntax-inline
      #`(bytevector
        #,@(map (partial datum->syntax #'+)
          (bytevector->u8-list $expr)))))
)
