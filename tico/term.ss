(library (tico term)
  (export
    variable variable? variable-index
    function function? function-arity function-body
    application application? application-function application-args
    term->datum scope-term->datum
    term->value)
  (import (micascheme))

  (data (variable index))
  (data (function arity body))
  (data (application function args))

  (define (term->value $term)
    (eval
      (term->datum $term)
      (environment `(micascheme))))

  (define (term->datum $term)
    (scope-term->datum (stack) $term))

  (define (scope-term->datum $scope $term)
    (switch $term
      ((variable? $variable)
        (list-ref $scope (variable-index $variable)))
      ((function? $function)
        (lets
          ($tmps
            (reverse
              (iterate
                (lambda ($tmps)
                  (push $tmps (syntax->datum (generate-temporary))))
                (stack)
                (function-arity $function))))
          `(lambda (,@$tmps)
            ,(scope-term->datum
              (push-list $scope $tmps)
              (function-body $function)))))
      ((application? $application)
        `(
          ,(scope-term->datum $scope (application-function $application))
          ,@(map (partial scope-term->datum $scope) (application-args $application))))
      ((else $other)
        $other)))
)
