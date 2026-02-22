(library (leo2 dependent)
  (export
    procedure-dependent?
    abstraction-dependent?
    signature-dependent?
    recursion-dependent?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal))

  (define (procedure-dependent? $procedure)
    (not
      (term=?
        (app $procedure (variable 'x))
        (app $procedure (variable 'y)))))

  (define (abstraction-dependent? $abstraction)
    (procedure-dependent?
      (abstraction-procedure $abstraction)))

  (define (signature-dependent? $signature)
    (procedure-dependent?
      (signature-procedure $signature)))

  (define (recursion-dependent? $recursion)
    (procedure-dependent?
      (recursion-procedure $recursion)))
)
