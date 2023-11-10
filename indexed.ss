(library (tt)
  (export)
  (import (micascheme))

  (data (typed term types))

  (data (native value types))

  (data (variable index))
  (data (abstraction params results))
  (data (application fn args))

  (data (field name values))
  (data (access tuple index))

  (data (choice values))
  (data (selection choice index values))
  (data (switch selection fns))

  (data (recursion type values))

  (define (type-static? $type) #f)

  (define (type-name $type)
    (switch $type
      ((symbol? $symbol) $symbol)
      ((field? $field) (field-name $field))
      ((abstraction? $abstraction) `lambda)
      ((choice? $choice) `choice)
      ((else $other) (throw type-name $type))

  (define (typed-name $typed)
    (type-name (typed-type $typed)))

  (define (type-generate-temporary $type)
    (generate-temporary (type-name $type)))

  (define (types-generate-temporaries $types)
    (map type-generate-temporary $types))

  (define (typed-list-bind-syntax $typed-list $fn)
    (lets
      ($terms (map typed-term $typed-list))
      ($types-list (map typed-types $typed-list))
      ($identifiers-list (map types-generate-temporaries $types-list))
      ($identifiers (flatten $identifiers-list))
      ($let-entries 
        (map 
          (lambda ($identifiers $term) #`((#,@$identifiers) $term))
          $identifiers-list $terms))
      #`(let-values (#,@$let-entries) ($fn $identifiers))))

  (define (typed-list-apply-syntax $typed-list $fn)
    (lets
      ($direct-terms-option (typed-list-direct-terms-option $typed-list))
      (if $direct-terms-option
        ($fn $direct-terms-option)
        (typed-list-bind-syntax $typed-list $fn))))

  (define (typed-direct-term-option $typed)
    (and (single? (typed-types $typed)) (typed-term $typed)))

  (define (typed-list-direct-terms-option $typed-list)
    (lets
      ($direct-term-options (map typed-direct-term-option $typed-list))
      (and 
        (for-all (lambda ($t) $t) $direct-term-options)
        $direct-term-options)))

  (define (typed-syntax $identifiers $typed)
    (lets
      ($term (typed-term $typed))
      ($type (typed-type $typed))
      ($switch $term
        ((native? $native) 
          (native-value $native))
        ((variable? $variable) 
          (list-ref $identifiers (variable-index $variable)))
        ((abstraction? $abstraction)
          (lets
            ($params (generate-temporaries (map term-name $params)))
            #`(lambda (#,@(generate-temporaries (map term-name $params)))
              (typed-list->syntax 
                (append $identifiers $env) 
                (abstraction-results $abstraction)))))
        ((application? $application) 
          (typed-list-apply-syntax 
            (application-args $application)
            (lambda ($arg-syntax-list)
              #`(
                #,(typed-syntax $identifiers (application-fn $application))
                #,@$arg-syntax-list))))
        ((field? $field)
          (typed-list-apply-syntax 
            (field-values $field)
            (lambda ($values-syntax-list)
              #`(field
                (quote #,(field-name $field))
                #,@$values-syntax-list))))
        ((access? $access)
          #`(list-ref 
            (field-values #,(typed-syntax $identifiers (access-tuple $access)))
            #,(access-index $access)))
        ((choice? $choice)
          (typed-list-apply-syntax 
            (choice-values $choice)
            (lambda ($values-syntax-list)
              #`(choice #,@$values-syntax-list))))
        ((selection? $selection)
          )
)
