(library (tt raw)
  (export)
  (import
    (scheme)
    (boolean)
    (data)
    (lets)
    (switch)
    (throw)
    (prefix (tt term) %)
    (prefix (tt primitive) %)
    (prefix (tt type) %))

  (data (kind index))
  (data (variable symbol))
  (data (abstraction variable domain body))
  (data (product variable? domain body))
  (data (application lhs rhs))

  (data (elaborated type value))

  (define empty-lookup
    (lambda ($symbol)
      (throw `(lookup ,$symbol))))

  (define (lookup-push $lookup $symbol $value)
    (lambda ($lookup-symbol)
      (cond
        ((symbol=? $symbol $lookup-symbol) $value)
        (else ($lookup $lookup-symbol)))))

  (define (term-elaborate $lookup $term)
    (switch $term
      ((boolean? $boolean)
        (elaborated
          (%class 'symbol)
          $boolean))
      ((number? $number)
        (elaborated
          (%class 'number)
          $number))
      ((char? $char)
        (elaborated
          (%class 'char)
          $char))
      ((string? $string)
        (elaborated
          (%class 'string)
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
          ($elaborated-domain (term-elaborate $lookup (abstraction-domain $abstraction)))
          ($lookup (lookup-push $lookup $variable $elaborated-domain))
          (elaborated
            (%product
              (elaborated-value $elaborated-domain)
              (lambda ($0)
                (elaborated-type
                  (term-elaborate $lookup
                    (abstraction-domain $abstraction)))))
            (%abstraction
              (lambda ($0)
                (elaborated-value
                  (term-elaborate $lookup
                    (abstraction-body $abstraction))))))))
      ((product? $product)
        (lets
          ($variable? (product-variable? $product))
          ($elaborated-domain (term-elaborate $lookup (product-domain $product)))
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
                  (term-elaborate $lookup
                    (product-body $product))))))))
      ((application? $application)
        (lets
          ($lhs (term-elaborate $lookup (application-lhs $application)))
          ($rhs (term-elaborate $lookup (application-rhs $application)))
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
      ((else $other)
        (throw `(term-elaborate ,$term)))))
)
