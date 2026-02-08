(library (micalang term)
  (export
    type type?
    native native? native-ref
    variable variable? variable-symbol
    constant constant? constant-ref
    tagged tagged? tagged-tag tagged-ref
    abstraction abstraction? abstraction-symbol abstraction-procedure abstraction-apply
    application application? application-lhs application-rhs
    pi pi? pi-symbol? pi-param pi-procedure pi-apply
    conditional conditional? conditional-cond conditional-true conditional-false

    term-apply
    apply-term
    default-term-equal? term-equal?)
  (import (micalang base))

  (data type)
  (data (native ref))
  (data (variable symbol))
  (data (constant ref))
  (data (tagged tag ref))
  (data (abstraction symbol procedure))
  (data (application lhs rhs))
  (data (pi symbol? param procedure))
  (data (conditional cond true false))

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
      ((pi? $pi)
        (pi-apply $pi $rhs))
      ((else $other)
        (application $other $rhs))))

  (define (term-equal? $lhs $rhs)
    (default-term-equal?
      (lambda ($default $lhs $rhs) #f)
      $lhs
      $rhs))

  (define (default-term-equal? $default $lhs $rhs)
    (switch $lhs
      ((type? _)
        (switch? $rhs
          ((type? _) #t)))
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
            (default-term-equal? $default
              (abstraction-apply $lhs-abstraction (variable (abstraction-symbol $lhs-abstraction)))
              (abstraction-apply $rhs-abstraction (variable (abstraction-symbol $lhs-abstraction)))))))
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
      ((pi? $lhs-pi)
        (switch? $rhs
          ((pi? $rhs-pi)
            (and
              (default-term-equal? $default
                (pi-param $lhs-pi)
                (pi-param $rhs-pi))
              (default-term-equal? $default
                (pi-apply $lhs-pi (variable (pi-symbol? $lhs-pi)))
                (pi-apply $rhs-pi (variable (pi-symbol? $lhs-pi))))))))
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
      ((else $other)
        ($default $default $lhs $rhs))))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (pi-apply $pi $arg)
    ((pi-procedure $pi) $arg))
)
