(library (leo2 dependent)
  (export
    abstraction-dependent?
    abstraction-type-dependent?
    recursive-dependent?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal))

  (define (abstraction-dependent? $abstraction)
    (not
      (term=? 0
        (abstraction-apply $abstraction (variable 'a))
        (abstraction-apply $abstraction (variable 'b)))))

  (define (abstraction-type-dependent? $abstraction-type)
    (not
      (term=? 0
        (abstraction-type-apply $abstraction-type (variable 'a))
        (abstraction-type-apply $abstraction-type (variable 'b)))))

  (define (recursive-dependent? $recursive)
    (not
      (term=? 0
        (recursive-apply $recursive (variable 'a))
        (recursive-apply $recursive (variable 'b)))))
)
