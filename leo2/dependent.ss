(library (leo2 dependent)
  (export
    procedure-dependent?
    procedure-type-dependent?
    recursion-dependent?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal))

  (define (procedure-dependent? $procedure)
    (not
      (term=?
        ($procedure (variable 0))
        ($procedure (variable 1)))))

  (define (procedure-type-dependent? $procedure-type)
    (procedure-dependent?
      (procedure-type-procedure $procedure-type)))

  (define (recursion-dependent? $recursion)
    (procedure-dependent?
      (recursion-procedure $recursion)))
)
