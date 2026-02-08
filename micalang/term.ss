(library (micalang term)
  (export
    type type?
    native native? native-ref
    variable variable? variable-symbol
    abstraction abstraction? abstraction-symbol abstraction-procedure abstraction-apply
    application application? application-lhs application-rhs
    pi pi? pi-symbol? pi-param pi-procedure pi-apply
    conditional conditional? conditional-cond conditional-true conditional-false

    term-apply
    apply-term
    term-equal?)
  (import (micalang base))

  (data type)
  (data (native ref))
  (data (variable symbol))
  (data (abstraction symbol procedure))
  (data (application lhs rhs))
  (data (pi symbol? param procedure))
  (data (conditional cond true false))

  (define (apply-term $procedure $rhs)
    (if (native? $rhs)
      (native ($procedure (native-ref $rhs)))
      (application (native $procedure) $rhs)))

  (define (term-apply $lhs $rhs)
    (switch $lhs
      ((native? $native)
        (apply-term (native-ref $native) $rhs))
      ((abstraction? $abstraction)
        (abstraction-apply $abstraction $rhs))
      ((pi? $pi)
        (pi-apply $pi $rhs))
      ((else $other)
        (application $other $rhs))))

  (define (term-equal? $lhs $rhs)
    (switch-exhaustive $lhs
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
      ((abstraction? $lhs-abstraction)
        (switch? $rhs
          ((abstraction? $rhs-abstraction)
            (term-equal?
              (abstraction-apply $lhs-abstraction (variable (abstraction-symbol $lhs-abstraction)))
              (abstraction-apply $rhs-abstraction (variable (abstraction-symbol $lhs-abstraction)))))))
      ((application? $lhs-application)
        (switch? $rhs
          ((application? $rhs-application)
            (and
              (term-equal?
                (application-lhs $lhs-application)
                (application-lhs $rhs-application))
              (term-equal?
                (application-rhs $lhs-application)
                (application-rhs $rhs-application))))))
      ((pi? $lhs-pi)
        (switch? $rhs
          ((pi? $rhs-pi)
            (and
              (term-equal?
                (pi-param $lhs-pi)
                (pi-param $rhs-pi))
              (term-equal?
                (pi-apply $lhs-pi (variable (pi-symbol? $lhs-pi)))
                (pi-apply $rhs-pi (variable (pi-symbol? $lhs-pi))))))))
      ((conditional? $lhs-conditional)
        (switch? $rhs
          ((conditional? $rhs-conditional)
            (and
              (term-equal?
                (conditional-cond $lhs-conditional)
                (conditional-cond $rhs-conditional))
              (term-equal?
                (conditional-true $lhs-conditional)
                (conditional-true $rhs-conditional))
              (term-equal?
                (conditional-false $lhs-conditional)
                (conditional-false $rhs-conditional))))))))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (pi-apply $pi $arg)
    ((pi-procedure $pi) $arg))
)
