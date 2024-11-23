(library (micalog flatten)
  (export
    check-flattens
    flatten-module)
  (import
    (micascheme)
    (micalog model)
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
    (syntax-case $item (%input %output %register %wire %on %set %assign %cond %else)
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
      ((%cond clause ... (%else body ...))
        (lets
          ($clause-pairs
            (map clause->declarations-clause (syntaxes clause ...)))
          ((pair $else-declarations $else-instrs)
            (items->declarations-instrs (syntaxes body ...)))
          (pair
            (append
              (flatten (map car $clause-pairs))
              $else-declarations)
            (list
              #`(%cond
                #,@(map cdr $clause-pairs)
                (%else #,@$else-instrs))))))
      ((%cond clause-1 clause ...)
        (lets
          ($clause-pairs
            (map clause->declarations-clause (syntaxes clause-1 clause ...)))
          (pair
            (flatten (map car $clause-pairs))
            (list #`(%cond #,@(map cdr $clause-pairs))))))
      ((%set body ...)
        (pair (list) (list $item)))))

  (define (clause->declarations-clause $clause)
    (syntax-case $clause ()
      ((cond body ...)
        (lets
          ((pair $declarations $instrs)
            (items->declarations-instrs (syntaxes body ...)))
          (pair
            $declarations
            #`(cond #,@$instrs))))))

  (define (process->declarations-process $item)
    (syntax-case $item ()
      ((edge item ...)
        (lets
          ((pair $declarations $instrs)
            (items->declarations-instrs (syntaxes item ...)))
          (pair $declarations #`(edge #,@$instrs))))))


)
