(library (micalog model)
  (export
    type-size
    expr-type
    expr-value
    reg-type)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define (type-size $type)
    (syntax-case $type ()
      (size
        (positive-integer? (datum size))
        #'size)))

  (define (reg-type $reg)
    (syntax-case $reg (%reg)
      ((%reg type) #'type)))

  (define (expr-type $expr)
    (syntax-case $expr (%expr)
      ((%expr type _) #'type)))

  (define (expr-value $expr)
    (syntax-case $expr (%expr)
      ((%expr _ value) #'value)))
)