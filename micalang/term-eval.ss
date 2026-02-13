(library (micalang term-eval)
  (export
    term-eval
    default-term-eval)
  (import
    (micalang base)
    (micalang term))

  (define (term-eval $term)
    (default-term-eval
      (lambda ($default $term)
        (throw term-eval))
      $term))

  (define (default-term-eval $default $term)
    (switch $term
      ((a-type? $a-type) $a-type)
      ((native? $native) $native)
      ((variable? $variable) $variable)
      ((constant? $constant)
        (constant
          (default-term-eval $default (constant-ref $constant))))
      ((tagged? $tagged)
        (tagged
          (default-term-eval $default (tagged-tag $tagged))
          (default-term-eval $default (tagged-ref $tagged))))
      ((abstraction? $abstraction) $abstraction)
      ((application? $application)
        (lets
          ($lhs (default-term-eval $default (application-lhs $application)))
          ($rhs (default-term-eval $default (application-rhs $application)))
          (switch $lhs
            ((abstraction? $abstraction)
              (default-term-eval $default (abstraction-apply $abstraction $rhs)))
            ((type-abstraction? $type-abstraction)
              (default-term-eval $default (type-abstraction-apply $type-abstraction $rhs)))
            ((native? $native)
              (switch $rhs
                ((native? $rhs)
                  (native ((native-ref $native) (native-ref $rhs))))
                ((else $other)
                  (application $native $rhs))))
            ((else $other)
              (application $lhs $rhs)))))
      ((type-abstraction? $type-abstraction) $type-abstraction)
      ((conditional? $conditional)
        (lets
          ($cond (default-term-eval $default (conditional-cond $conditional)))
          (switch $cond
            ((boolean? $cond)
              (if $cond
                (default-term-eval $default (conditional-true $conditional))
                (default-term-eval $default (conditional-false $conditional))))
            ((else $other)
              (conditional
                $cond
                (conditional-true $conditional)
                (conditional-false $conditional))))))
      ((else $other)
        ($default $default $other))))
)
