(library (micalog utils)
  (export
    type-size
    expr-type
    expr-value
    reg-type
    opposite-edges?
    process-edge
    opposite-processes?
    declaration-kind-of?
    declaration-syntaxes-of
    declaration-name
    declaration-type
    edge-identifier
    edge=?
    edge+?)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define (declaration-kind-of? $kind $declaration)
    (syntax-case $declaration ()
      ((kind body ...)
        (free-identifier=? #'kind $kind))
      (_ #f)))

  (define-rule-syntax (declaration-syntaxes-of kind declaration ...)
    (filter
      (partial declaration-kind-of? #'kind)
      (syntaxes declaration ...)))

  (define (opposite-edges? $edge $other-edge)
    (syntax-case #`(#,$edge #,$other-edge) (%posedge %negedge)
      ((%posedge %negedge) #t)
      ((%negedge %posedge) #t)
      ((_ other) (syntax-error $other-edge "non opposite"))))

  (define (process-edge $process)
    (syntax-case $process ()
      ((edge body ...) #'edge)))

  (define (opposite-processes? $process $other-process)
    (opposite-edges?
      (process-edge $process)
      (process-edge $other-process)))

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

  (define (declaration-name $declaration)
    (syntax-case $declaration ()
      ((kind type name) #'name)))

  (define (declaration-type $declaration)
    (syntax-case $declaration ()
      ((kind type name) #'type)))

  (define (edge-identifier $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge #'%posedge)
      (%negedge #'%negedge)
      (_ (syntax-error $edge "invalid edge"))))

  (define (edge=? $edge-a $edge-b)
    (free-identifier=?
      (edge-identifier $edge-a)
      (edge-identifier $edge-b)))

  (define (edge+? $edge-a $edge-b)
    (and
      (edge=? $edge-a $edge-b)
      $edge-a))
)
