(library (decompiler)
  (export decompile)
  (import
    (except (micascheme) pair)
    (term)
    (typed))

  (define (decompile $typed)
    (lets 
      ($value (typed-value $typed))
      ($type (typed-type $typed))
      (switch $type
        ((symbol? $symbol) 
          (case $symbol
            ((boolean) boolean!)
            ((number) number!)
            ((string) string!)
            ((type) type!)
            (else $symbol)))
        ((boolean-type? _) $value)
        ((number-type? _) $value)
        ((string-type? _) $value)
        ((tuple-type? $tuple-type)
          (lets
            ($name (tuple-type-name $tuple-type))
            ($types (tuple-type-types $tuple-type))
            ($decompiled-types 
              (case (length $types)
                ((0) 
                  (list))
                ((1) 
                  (list 
                    (decompile (typed $value (car $types)))))
                ((2)
                  (list 
                    (decompile (typed (car $value) (car $types)))
                    (decompile (typed (cdr $value) (cadr $types)))))
                (else 
                  (map decompile 
                    (map typed (vector->list $value) $types)))))
            (case $name
              ((choice) (choice-type $decompiled-types))
              (else (tuple-type $name $decompiled-types)))))
        ((else $other) $other))))
)
