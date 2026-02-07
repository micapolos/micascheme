(library (micalang term)
  (export
    native native? native-ref
    variable variable? variable-index
    abstraction abstraction? abstraction-symbol abstraction-procedure abstraction-apply
    application application? application-lhs application-rhs
    pi pi? pi-symbol? pi-param pi-procedure pi-apply
    conditional conditional? conditional-cond conditional-true conditional-false

    term-apply
    apply-term
    term-equal?)
  (import (micalang base))

  (data (native ref))
  (data (variable index))
  (data (abstraction symbol procedure))
  (data (application lhs rhs))
  (data (pi symbol? param procedure))
  (data (conditional cond true false))

  (define (apply-term $procedure $rhs)
    (if (native? $rhs)
      ($procedure (native-ref $rhs))
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

  (define (depth-term-equal? $depth $lhs $rhs)
    (switch-exhaustive $lhs
      ((native? $lhs-native)
        (switch? $rhs
          ((native? $rhs-native)
            (equal?
              (native-ref $lhs-native)
              (native-ref $rhs-native)))))
      ((variable? $lhs-variable)
        (switch? $rhs
          ((variable? $rhs-variable)
            (=
              (variable-index $lhs-variable)
              (variable-index $rhs-variable)))))
      ((abstraction? $lhs-abstraction)
        (switch? $rhs
          ((abstraction? $rhs-abstraction)
            (depth-term-equal? (+ $depth 1)
              (abstraction-apply $lhs-abstraction (variable $depth))
              (abstraction-apply $rhs-abstraction (variable $depth))))))
      ((application? $lhs-application)
        (switch? $rhs
          ((application? $rhs-application)
            (and
              (depth-term-equal? $depth
                (application-lhs $lhs-application)
                (application-lhs $rhs-application))
              (depth-term-equal? $depth
                (application-rhs $lhs-application)
                (application-rhs $rhs-application))))))
      ((pi? $lhs-pi)
        (switch? $rhs
          ((pi? $rhs-pi)
            (and
              (depth-term-equal? $depth
                (pi-param $lhs-pi)
                (pi-param $rhs-pi))
              (depth-term-equal? (+ $depth 1)
                (pi-apply $lhs-pi (variable $depth))
                (pi-apply $rhs-pi (variable $depth)))))))
      ((conditional? $lhs-conditional)
        (switch? $rhs
          ((conditional? $rhs-conditional)
            (and
              (depth-term-equal? $depth
                (conditional-cond $lhs-conditional)
                (conditional-cond $rhs-conditional))
              (depth-term-equal? $depth
                (conditional-true $lhs-conditional)
                (conditional-true $rhs-conditional))
              (depth-term-equal? $depth
                (conditional-false $lhs-conditional)
                (conditional-false $rhs-conditional))))))))

  (define (abstraction-apply $abstraction $arg)
    ((abstraction-procedure $abstraction) $arg))

  (define (pi-apply $pi $arg)
    ((pi-procedure $pi) $arg))

  (define (term-equal? $lhs $rhs)
    (depth-term-equal? 0 $lhs $rhs))
)
