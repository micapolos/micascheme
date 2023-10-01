(library (tico term)
  (export
    variable variable? variable-index
    function function? function-arity function-body
    application application? application-function application-args
    term->datum
    term->value
    term->free-variable-count)
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
          ($tmps (gen-list generate-symbol (function-arity $function)))
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

  (define (term->free-variable-count $term)
    (depth-term->free-variable-count 0 $term))

  (define (depth-term->free-variable-count $depth $term)
    (switch $term
      ((variable? $variable)
        (max 0 (- (variable-index $variable) $depth -1)))
      ((function? $function)
        (depth-term->free-variable-count
          (+ $depth (function-arity $function))
          (function-body $function)))
      ((application? $application)
        (max
          (depth-term->free-variable-count $depth (application-function $application))
          (apply max
            (map
              (partial depth-term->free-variable-count $depth)
              (application-args $application)))))
      ((else $other) 0)))
)
