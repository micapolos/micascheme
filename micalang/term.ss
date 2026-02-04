(library (micalang term)
  (export
    native native? native-ref
    variable variable? variable-index
    abstraction abstraction? abstraction-procedure
    application application? application-lhs application-rhs
    pi pi? pi-param pi-procedure
    conditional conditional? conditional-cond conditional-true conditional-false

    depth-term->datum
    check-term->datum

    term-apply
    term-equal?)
  (import (except (micascheme) pi))

  (data (native ref))
  (data (variable index))
  (data (abstraction procedure))
  (data (application lhs rhs))
  (data (pi param procedure))
  (data (conditional cond true false))

  (define (index->symbol $index)
    (string->symbol (format "v~a" $index)))

  (define (depth-term->datum $depth $term)
    (switch-exhaustive $term
      ((native? $native)
        (native-ref $native))
      ((variable? $variable)
        (index->symbol (variable-index $variable)))
      ((abstraction? $abstraction)
        (lets
          ($variable (variable $depth))
          `(lambda (,(index->symbol $depth))
            ,(depth-term->datum
              (+ $depth 1)
              ((abstraction-procedure $abstraction) $variable)))))
      ((application? $application)
        `(
          ,(depth-term->datum $depth (application-lhs $application))
          ,(depth-term->datum $depth (application-rhs $application))))
      ((pi? $pi)
        (lets
          ($param-datum (depth-term->datum $depth (pi-param $pi)))
          ($variable (variable $depth))
          ($symbol (index->symbol $depth))
          ($procedure (pi-procedure $pi))
          ($body-depth (+ $depth 1))
          ($body-datum (depth-term->datum $body-depth ($procedure $variable)))
          ($hole-body-datum (depth-term->datum $body-depth ($procedure (native 'hole))))
          `(pi
            ,(if (equal? $body-datum $hole-body-datum)
              $param-datum
              `(,$symbol : ,$param-datum))
            ,$body-datum)))
      ((conditional? $conditional)
        `(if
          ,(depth-term->datum $depth (conditional-cond $conditional))
          ,(depth-term->datum $depth (conditional-true $conditional))
          ,(depth-term->datum $depth (conditional-false $conditional))))))

  (define-rule-syntax (check-term->datum in out)
    (check (equal? (depth-term->datum 0 in) `out)))

  (define (term-apply $procedure $rhs)
    (if (native? $rhs)
      ($procedure $rhs)
      (application $procedure $rhs)))

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
              ((abstraction-procedure $lhs-abstraction) (variable $depth))
              ((abstraction-procedure $rhs-abstraction) (variable $depth))))))
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
                ((pi-procedure $lhs-pi) (variable $depth))
                ((pi-procedure $rhs-pi) (variable $depth)))))))
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

  (define (term-equal? $lhs $rhs)
    (depth-term-equal? 0 $lhs $rhs))
)
