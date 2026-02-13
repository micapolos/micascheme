(library (micalang term)
  (export
    a-type a-type?
    native native? native-ref
    variable variable? variable-symbol
    constant constant? constant-ref
    tagged tagged? tagged-tag tagged-ref
    abstraction abstraction? abstraction-symbol? abstraction-param abstraction-procedure abstraction-apply
    application application? application-lhs application-rhs
    type-abstraction type-abstraction? type-abstraction-symbol? type-abstraction-param type-abstraction-procedure type-abstraction-apply
    conditional conditional? conditional-cond conditional-true conditional-false
    macro macro? macro-procedure macro-apply

    term-apply
    apply-term
    default-term-equal? term-equal?)
  (import (micalang base))

  (data a-type)
  (data (native ref))
  (data (variable symbol))
  (data (constant ref))
  (data (tagged tag ref))
  (data (abstraction symbol? param procedure))
  (data (application lhs rhs))
  (data (type-abstraction symbol? param procedure))
  (data (conditional cond true false))
  (data (macro procedure))

  (define (apply-term $procedure $rhs)
    (switch $rhs
      ((native? $native)
        (native ($procedure (native-ref $native))))
      ((tagged? $tagged)
        (apply-term $procedure (tagged-ref $tagged)))
      ((else $other)
        (application (native $procedure) $other))))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((native? $native)
        (apply-term (native-ref $native) $rhs))
      ((tagged? $tagged)
        (term-apply (tagged-ref $tagged) $rhs))
      ((abstraction? $abstraction)
        (abstraction-apply $abstraction $rhs))
      ((type-abstraction? $type-abstraction)
        (type-abstraction-apply $type-abstraction $rhs))
      ((else $other)
        (application $other $rhs))))

  (define (term-equal? $lhs $rhs)
    (default-term-equal?
      (lambda ($default $lhs $rhs) (throw term-equal? $lhs $rhs))
      $lhs
      $rhs))

  (define (default-term-equal? $default $lhs $rhs)
    (switch $lhs
      ((a-type? _)
        (switch? $rhs
          ((a-type? _) #t)))
      ((native? $lhs-native)
        (switch? $rhs
          ((native? $rhs-native)
            (equal?
              (native-ref $lhs-native)
              (native-ref $rhs-native)))))
      ((variable? $lhs-variable)
        (switch? $rhs
          ((variable? $rhs-variable)
            (symbol=?
              (variable-symbol $lhs-variable)
              (variable-symbol $rhs-variable)))))
      ((constant? $lhs-constant)
        (switch? $rhs
          ((constant? $rhs-constant)
            (equal?
              (constant-ref $lhs-constant)
              (constant-ref $rhs-constant)))))
      ((tagged? $lhs-tagged)
        (switch? $rhs
          ((tagged? $rhs-tagged)
            (and
              (default-term-equal? $default
                (tagged-tag $lhs-tagged)
                (tagged-tag $rhs-tagged))
              (default-term-equal? $default
                (tagged-ref $lhs-tagged)
                (tagged-ref $rhs-tagged))))))
      ((abstraction? $lhs-abstraction)
        (switch? $rhs
          ((abstraction? $rhs-abstraction)
            (and
              (default-term-equal? $default
                (abstraction-param $lhs-abstraction)
                (abstraction-param $rhs-abstraction))
              (default-term-equal? $default
                (abstraction-apply $lhs-abstraction (variable (abstraction-symbol? $lhs-abstraction)))
                (abstraction-apply $rhs-abstraction (variable (abstraction-symbol? $lhs-abstraction))))))))
      ((application? $lhs-application)
        (switch? $rhs
          ((application? $rhs-application)
            (and
              (default-term-equal? $default
                (application-lhs $lhs-application)
                (application-lhs $rhs-application))
              (default-term-equal? $default
                (application-rhs $lhs-application)
                (application-rhs $rhs-application))))))
      ((type-abstraction? $lhs-pi)
        (switch? $rhs
          ((type-abstraction? $rhs-pi)
            (and
              (default-term-equal? $default
                (type-abstraction-param $lhs-pi)
                (type-abstraction-param $rhs-pi))
              (default-term-equal? $default
                (type-abstraction-apply $lhs-pi (variable (type-abstraction-symbol? $lhs-pi)))
                (type-abstraction-apply $rhs-pi (variable (type-abstraction-symbol? $lhs-pi))))))))
      ((conditional? $lhs-conditional)
        (switch? $rhs
          ((conditional? $rhs-conditional)
            (and
              (default-term-equal? $default
                (conditional-cond $lhs-conditional)
                (conditional-cond $rhs-conditional))
              (default-term-equal? $default
                (conditional-true $lhs-conditional)
                (conditional-true $rhs-conditional))
              (default-term-equal? $default
                (conditional-false $lhs-conditional)
                (conditional-false $rhs-conditional))))))
      ((macro? $lhs-macro)
        (switch? $rhs
          ((macro? $rhs-macro)
            (eq?
              (macro-procedure $lhs-macro)
              (macro-procedure $rhs-macro)))))
      ((else $other)
        ($default $default $lhs $rhs))))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (type-abstraction-apply $type-abstraction $arg)
    ((type-abstraction-procedure $type-abstraction) $arg))

  (define (macro-apply $macro $compiler $term)
    ((macro-procedure $macro) $compiler $term))
)
