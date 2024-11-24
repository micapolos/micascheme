(library (micalog core utils)
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
    edge+?
    edges+
    event+?
    domain+)
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

  (define (edges+ $edges-a $edges-b)
    (syntax-case #`(#,$edges-a #,$edges-b) (%posedge %negedge %edge)
      ((%posedge %posedge) #'%posedge)
      ((%negedge %negedge) #'%negedge)
      ((_ _) #'%edge)))

  (define (event+? $event-a $event-b)
    (syntax-case #`(#,$event-a #,$event-b) ()
      (((edges-a signal-a) (edges-b signal-b))
        (and
          (free-identifier=? #'signal-a #'signal-b)
          #`(#,(edges+ #'edges-a #'edges-b) signal-a)))))

  (define event=? syntax=?)

  (define (domain+ $domain-a $domain-b)
    (syntax-case #`(#,$domain-a #,$domain-b) ()
      ((() _) #'())
      ((_ ()) #'())
      (((event-a . events-a) (event-b . events-b))
        (lets
          ($event? (event+? #'event-a #'event-b))
          (if $event?
            #`(#,$event?
              #,@(if (event=? $event? #'event-a)
                (domain+ #'events-a #'events-b)
                (list)))
            #'())))))
)
