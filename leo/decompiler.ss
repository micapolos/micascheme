(library (leo decompiler)
  (export 
    decompiler decompiler? decompiler-types
    empty-decompiler
    decompile)
  (import 
    (except (micascheme) function pair)
    (leo value))

  (data (decompiler types))

  (define (empty-decompiler)
    (decompiler (stack)))

  (define (decompile $decompiler $typed)
    (lets
      ($value (typed-value $typed))
      ($type (typed-type $typed))
      (switch $type
        ((any-boolean? _) $value)
        ((any-number? _) $value)
        ((any-string? _) $value)
        ((named? $named)
          (named
            (named-name $named)
            (decompile $decompiler 
              (typed $value (named-value $named)))))
        ((tuple? $tuple)
          (tuple
            (lets
              ($types (tuple-items $tuple))
              (map (partial decompile $decompiler)
                (map typed
                  (case (length $types)
                    ((0) (list))
                    ((1) (list $value))
                    ((2) (list (car $value) (cdr $value)))
                    (else (vector->list $value)))
                  $types)))))
        ((choice? $choice)
          (lets
            ($types (choice-items $choice))
            (case (length $types)
              ((0) 
                (throw empty-choice))
              ((1)
                (decompile $decompiler (typed $value (car $types))))
              ((2) 
                (decompile $decompiler
                  (typed
                    (cdr $value)
                    (if (car $value) (car $types) (cadr $types)))))
              (else
                (decompile $decompiler
                  (typed
                    (cdr $value)
                    (list-ref $types (car $value))))))))
        ((else $other)
          (typed $value $type)))))
)
