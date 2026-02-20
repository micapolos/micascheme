(library (leo2 dependent)
  (export
    abstraction-dependent?
    abstraction-type-dependent?
    recursion-dependent?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 equal)
    (leo2 stdlib))

  (define (abstraction-dependent? $abstraction)
    (not
      (term=? 0
        (abstraction-apply $abstraction
          (variable-term string-type (variable 'a)))
        (abstraction-apply $abstraction
          (variable-term string-type (variable 'b))))))

  (define (abstraction-type-dependent? $abstraction-type)
    (not
      (term=? 0
        (abstraction-type-apply $abstraction-type
          (variable-term string-type (variable 'a)))
        (abstraction-type-apply $abstraction-type
          (variable-term string-type (variable 'b))))))

  (define (recursion-dependent? $recursion)
    (not
      (term=? 0
        (recursion-apply $recursion
          (variable-term string-type (variable 'a)))
        (recursion-apply $recursion
          (variable-term string-type (variable 'b))))))
)
