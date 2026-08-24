(library (tt raw)
  (export
    kind
    kind?
    kind-index

    variable
    variable?
    variable-symbol

    abstraction
    abstraction?
    abstraction-variable
    abstraction-domain
    abstraction-body

    product
    product?
    product-variable?
    product-domain
    product-body

    application
    application?
    application-lhs
    application-rhs

    elaborated
    elaborated?
    elaborated-type
    elaborated-value
    elaborated->datum
    check-elaborated

    empty-lookup
    lookup-push
    lookup

    elaborate)
  (import
    (scheme)
    (check)
    (boolean)
    (data)
    (lets)
    (switch)
    (throw)
    (syntax)
    (procedure)
    (prefix (tt term) %)
    (prefix (tt primitive) %)
    (prefix (tt type) %))

  (data (kind index))
  (data (variable symbol))
  (data (abstraction variable domain body))
  (data (product variable? domain body))
  (data (application lhs rhs))
  (data (class symbol))
  (data (elaborated type value))

  (define (elaborated->datum $elaborated)
    `(elaborated
      ,(%type->datum (elaborated-type $elaborated))
      ,(%type->datum (elaborated-value $elaborated))))

  (define-rule-syntax (check-elaborated lookup term out)
    (check
      (equal?
        (elaborated->datum (elaborate lookup term))
        (elaborated->datum out))))

  (define empty-lookup
    (lambda ($symbol)
      (throw `(lookup ,$symbol))))

  (define (lookup-push $lookup $symbol $value)
    (lambda ($lookup-symbol)
      (cond
        ((symbol=? $symbol $lookup-symbol) $value)
        (else ($lookup $lookup-symbol)))))

  (define-rule-syntax (lookup (key value) ...)
    (fold-left
      lookup-push
      empty-lookup
      '(key ...)
      (list value ...)))

  (define (elaborate* $lookup $terms)
    (map (partial elaborate $lookup) $terms))

  (define (elaborate $lookup $term)
    (switch $term
      ((elaborated? $elaborated)
        $elaborated)
      ((boolean? $boolean)
        (elaborated
          %boolean-type-constructor
          $boolean))
      ((number? $number)
        (elaborated
          %number-type-constructor
          $number))
      ((char? $char)
        (elaborated
          %char-type-constructor
          $char))
      ((string? $string)
        (elaborated
          %string-type-constructor
          $string))
      ((%class? $class)
        (elaborated
          (%kind 0)
          $class))
      ((kind? $kind)
        (elaborated
          (%kind (+ (kind-index $kind) 1))
          (%kind (kind-index $kind))))
      ((variable? $variable)
        ($lookup (variable-symbol $variable)))
      ((abstraction? $abstraction)
        (lets
          ($variable (abstraction-variable $abstraction))
          ($symbol (variable-symbol $variable))
          ($domain (elaborated-value (elaborate $lookup (abstraction-domain $abstraction))))
          (elaborated
            (%product $domain
              (lambda ($0)
                (elaborated-type
                  (elaborate
                    (lookup-push $lookup $symbol (elaborated $domain $0))
                    (abstraction-body $abstraction)))))
            (%abstraction
              (lambda ($0)
                (elaborated-value
                  (elaborate
                    (lookup-push $lookup $symbol (elaborated $domain $0))
                    (abstraction-body $abstraction))))))))
      ((product? $product)
        (lets
          ($variable? (product-variable? $product))
          ($elaborated-domain (elaborate $lookup (product-domain $product)))
          ($lookup
            (if $variable?
              (lookup-push $lookup $variable? $elaborated-domain)
              $lookup))
          (elaborated
            (%kind 0)
            (%product
              (elaborated-value $elaborated-domain)
              (lambda ($0)
                (elaborated-value
                  (elaborate $lookup
                    (product-body $product))))))))
      ((application? $application)
        (lets
          ($lhs (elaborate $lookup (application-lhs $application)))
          ($rhs (elaborate $lookup (application-rhs $application)))
          ((values $subst $lhs-type) (%type-instantiate (elaborated-type $lhs)))
          (switch $lhs-type
            ((%product? $product)
              (lets
                ($subst
                  (%type-unify $subst
                    (%product-domain $product)
                    (elaborated-type $rhs)))
                (elaborated
                  (%type-finalize $subst
                    (%product-apply $product
                      (elaborated-value $rhs)))
                  (%abstraction-apply
                    (elaborated-value $lhs)
                    (elaborated-value $rhs)))))
            ((else $other)
              (throw `(not-lambda ,$lhs))))))
      ((%type-constructor? $type-constructor)
        (elaborated
          (%kind 0)
          (%type-constructor
            (%type-constructor-symbol $type-constructor)
            (elaborate* $lookup (%type-constructor-args $type-constructor)))))
      ((else $other)
        (throw `(elaborate ,$term)))))
)
