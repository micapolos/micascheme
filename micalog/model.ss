(library (micalog model)
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
    items->declarations-instrs
    expand-module
    check-flattens
    declaration-name
    declaration-type)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define-rule-syntax (check-flattens in out)
    (check (equal? (syntax->datum (expand-module #'in)) 'out)))

  (define (expand-module $module)
    (syntax-case $module (%module)
      ((%module name item ...)
        #`(%module name
          #,@(expand-items
            (syntaxes item ...))))))

  (define (expand-items $items)
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
    (syntax-case $item (%input %output %register %wire %on %set %assign %when %if %then %else)
      ((%input body ...)
        (pair (list $item) (list)))
      ((%output type name)
        (pair
          (list #`(%output type name))
          (list)))
      ((%output type name expr)
        (pair
          (list
            #`(%output type name)
            #`(%assign type name expr))
          (list)))
      ((%register body ...)
        (pair (list $item) (list)))
      ((%wire type name)
        (pair
          (list #`(%wire type name))
          (list)))
      ((%wire type name expr)
        (pair
          (list
            #`(%wire type name)
            #`(%assign type name expr))
          (list)))
      ((%assign type name expr)
        (pair
          (list #`(%assign type name expr))
          (list)))
      ((%on name process)
        (lets
          ((pair $declarations $process)
            (process->declarations-process #'process))
          (pair
            (append $declarations (list #`(%on name #,$process)))
            (list))))
      ((%on name process opposite-process)
        (lets
          ((pair $declarations $process)
            (process->declarations-process #'process))
          ((pair $opposite-declarations $opposite-process)
            (process->declarations-process #'opposite-process))
          (pair
            (append
              $declarations
              $opposite-declarations
              (list
                #`(%on name #,$process)
                #`(%on name #,$opposite-process)))
            (list))))
      ((%when cond body ...)
        (lets
          ((pair $declarations $instrs)
            (items->declarations-instrs (syntaxes body ...)))
          (pair $declarations
            (list #`(%when cond #,@$instrs)))))
      ((%if cond (%then then ...) (%else els ...))
        (lets
          ((pair $then-declarations $then-instrs)
            (items->declarations-instrs (syntaxes then ...)))
          ((pair $else-declarations $else-instrs)
            (items->declarations-instrs (syntaxes els ...)))
          (pair
            (append $then-declarations $else-declarations)
            (list #`(%if cond (%then #,@$then-instrs) (%else #,@$else-instrs))))))
      ((%set body ...)
        (pair (list) (list $item)))))

  (define (process->declarations-process $item)
    (syntax-case $item ()
      ((edge item ...)
        (lets
          ((pair $declarations $instrs)
            (items->declarations-instrs (syntaxes item ...)))
          (pair $declarations #`(edge #,@$instrs))))))

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
)
