(library (micalog model)
  (export
    type-size
    expr-type
    expr-value
    reg-type
    opposite-edges?
    process-edge
    opposite-processes?
    flatten-declaration
    flatten-declarations
    declaration-kind-of?
    declaration-syntaxes-of
    items->declarations-instrs
    flatten-module
    check-flattens)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define-rule-syntax (check-flattens in out)
    (check (equal? (syntax->datum (flatten-module #'in)) 'out)))

  (define (flatten-module $module)
    (syntax-case $module (%module)
      ((%module name item ...)
        #`(%module name
          #,@(flatten-items
            (syntaxes item ...))))))

  (define (flatten-items $items)
    (lets
      ((pair $declarations $instrs) (items->declarations-instrs $items))
      (append $declarations $instrs)))

  (define (items->declarations-instrs $items)
    (lets
      ($pairs (map item->declarations-instrs $items))
      (pair
        (flatten (map car $pairs))
        (flatten (map cdr $pairs)))))

  (define (item->declarations-instrs $item)
    (syntax-case $item (%input %output %register %wire %on %set %assign)
      ((%input body ...)
        (pair (list $item) (list)))
      ((%output body ...)
        (pair (list $item) (list)))
      ((%register body ...)
        (pair (list $item) (list)))
      ((%wire body ...)
        (pair (list $item) (list)))
      ((%assign body ...)
        (pair (list $item) (list)))
      ((%on name process)
        (lets
          ((pair $declarations $process) (process->declarations-process #'process))
          (pair
            (append $declarations (list #`(%on name #,$process)))
            (list))))
      ((%on name process opposite-process)
        (lets
          ((pair $declarations $process) (process->declarations-process #'process))
          ((pair $opposite-declarations $opposite-process) (process->declarations-process #'opposite-process))
          (pair
            (append
              $declarations
              $opposite-declarations
              (list
                #`(%on name #,$process)
                #`(%on name #,$opposite-process)))
            (list))))
      ((%set body ...)
        (pair (list) (list $item)))))

  (define (process->declarations-process $item)
    (syntax-case $item ()
      ((edge item ...)
        (lets
          ((pair $declarations $instrs)
            (items->declarations-instrs (syntaxes item ...)))
          (pair $declarations #`(edge #,@$instrs))))))

  (define (flatten-declarations $declarations)
    (flatten (map flatten-declaration $declarations)))

  (define (flatten-declaration $declaration)
    (syntax-case $declaration (%input %output %register %wire %assign %on)
      ((%input body ...)
        (list $declaration))
      ((%output body ...)
        (list $declaration))
      ((%register body ...)
        (list $declaration))
      ((%wire body ...)
        (list $declaration))
      ((%assign body ...)
        (list $declaration))
      ((%on name process)
        (cons $declaration
          (process-declarations #'process)))
      ((%on name process opposite-process)
        (cons $declaration
          (append
            (process-declarations #'process)
            (process-declarations #'opposite-process))))
      (_ (list))))

  (define (process-declarations $process)
    (syntax-case $process ()
      ((edge body ...)
        (flatten-declarations
          (syntaxes body ...)))))

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
)
