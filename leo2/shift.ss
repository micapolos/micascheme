(library (leo2 shift)
  (export shift)
  (import
    (leo2 base)
    (leo2 term))

  (define (shift $threshold $term)
    (switch-exhaustive $term
      ((normalized? $normalized)
        (normalized
          (shift
            $threshold
            (normalized-value $term))))
      ((native? $native)
        $native)
      ((native-application? $native-application)
        (native-application
          (native-application-procedure $native-application)
          (map
            (partial shift $threshold)
            (native-application-args $native-application))))
      ((variable? $variable)
        (lets
          ($index (variable-index $variable))
          (if (>= $index $threshold)
            (variable (+ $index 1))
            $variable)))
      ((abstraction? $abstraction)
        (abstraction
          (shift
            (+ $threshold 1)
            (abstraction-body $abstraction))))
      ((abstraction-type? $abstraction-type)
        (abstraction-type
          (shift
            $threshold
            (abstraction-type-param $abstraction-type))
          (shift
            (+ $threshold 1)
            (abstraction-type-body $abstraction-type))))
      ((recursive? $recursive)
        (recursive
          (shift
            (+ $threshold 1)
            (recursive-body $recursive))))
      ((application? $application)
        (application
          (shift $threshold (application-lhs $application))
          (shift $threshold (application-rhs $application))))
      ((branch? $branch)
        (branch
          (shift $threshold (branch-condition $branch))
          (shift $threshold (branch-consequent $branch))
          (shift $threshold (branch-alternate $branch))))))
)
