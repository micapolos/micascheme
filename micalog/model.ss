(library (micalog model)
  (export
    type-size
    expr-type
    expr-value
    reg-type
    opposite-edges?
    process-edge
    opposite-processes?)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define (flatten-declarations $declarations)
    (flatten (map flatten-declaration $declarations)))

  (define (flatten-declaration $declaration)
    (syntax-case $declaration (%input %register %wire %on)
      ((%input body ...)
        (list #'%input))
      ((%register body ...)
        (list #'%register))
      ((%wire body ...)
        (list #'%wire))
      ((%on name process)
        (process-declarations #'process))
      ((%on name process opposite-process)
        (append
          (process-declarations #'process)
          (process-declarations #'opposite-process)))
      (_ (list))))

  (define (process-declarations $process)
    (syntax-case $process ()
      ((edge declaration ...)
        (flatten-declarations (syntaxes declaration ...)))))

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
)
