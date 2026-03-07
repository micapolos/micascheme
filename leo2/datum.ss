(library (leo2 datum)
  (export
    native->datum
    term->datum
    depth-term->datum
    recurse-depth-term->datum
    check-term->datum=?
    check-term-datum=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol)
    (procedure-name))

  (define (term->datum $term)
    (depth-term->datum 0 $term))

  (define (native->datum $ref)
    (switch $ref
      ((lambda? $lambda)
        (or
          (procedure-name? $lambda)
          $lambda))
      ((symbol? $symbol)
        `',$symbol)
      ((literal? $literal)
        $literal)
      ((record? $record)
        (record-type-name (record-rtd $record)))
      ((else $other)
        `',$other)))

  (define (depth-term->datum $depth $term)
    (recurse-depth-term->datum depth-term->datum $depth $term))

  (define (recurse-depth-term->datum $recurse $depth $term)
    ; TODO: Integrate in term-switch
    (switch $term
      ((primitive? $primitive)
        $primitive)
      ((pair? $pair)
        (pair-map (partial $recurse $depth) $pair))
      ((vector? $vector)
        (vector-map (partial $recurse $depth) $vector))
      ((mismatch? $mismatch)
        `(mismatch
          ,($recurse $depth (mismatch-expected $mismatch))
          ,($recurse $depth (mismatch-actual $mismatch))))
      ((expected? $expected)
        `(expected
          ,($recurse $depth (expected-ref $expected))))
      ((actual? $actual)
        `(actual
          ,($recurse $depth (actual-ref $actual))))
      ((unbound? $unbound)
        `(unbound
          ,($recurse $depth (unbound-ref $unbound))))
      ((native-type? _)
        `native-type)
      ((unknown? _)
        'unknown)
      ((error? $error)
        `(error ,($recurse $depth (error-ref $error))))
      ((else $term)
        (term-switch $term
          ((hole? $hole) `(hole ,(hole-index $hole)))
          ((nothing? _) 'nothing)
          ((type? $type)
            `(type ,(type-depth $type)))
          ((native? $native)
            `(native ,(native->datum (native-ref $native))))
          ((native-application? $native-application)
            `(native-application
              ,(native->datum (native-application-lambda $native-application))
              (list
                ,@(map
                  (partial $recurse $depth)
                  (native-application-args $native-application)))))
          ((variable? $variable)
            (lets
              ($index (variable-index $variable))
              (if (>= $index $depth)
                `(variable ,$index)
                (depth->symbol $index))))
          ((lambda? $lambda)
            (lets
              ($symbol (depth->symbol $depth))
              `(lambda (,(depth->symbol $depth))
                ,($recurse
                  (+ $depth 1)
                  ($lambda (variable $depth))))))
          ((lambda-type? $lambda-type)
            `(lambda-type
              ,($recurse $depth (lambda-type-param $lambda-type))
              ,($recurse $depth (lambda-type-lambda $lambda-type))))
          ((application? $application)
            `(application
              ,($recurse $depth (application-lhs $application))
              ,($recurse $depth (application-rhs $application))))
          ((branch? $branch)
            `(branch
              ,($recurse $depth (branch-condition $branch))
              ,($recurse $depth (branch-consequent $branch))
              ,($recurse $depth (branch-alternate $branch))))
          ((recursion? $recursion)
            `(recursion
              ,($recurse $depth (recursion-lambda $recursion))))
          ((labeled? $labeled)
            `(labeled
              ,($recurse $depth (labeled-label $labeled))
              ,($recurse $depth (labeled-ref $labeled))))
          ((evaluated? $evaluated)
            `(evaluated
              ,($recurse $depth (evaluated-ref $evaluated))))
          ((typed? $typed)
            `(typed
              ,($recurse $depth (typed-type $typed))
              ,($recurse $depth (typed-ref $typed))))))))

  (define-rule-syntax (check-term->datum=? in out)
    (check (equal? (term->datum in) `out)))

  (define-rule-syntax (check-term-datum=? in out)
    (check
      (equal?
        (term->datum in)
        (term->datum out))))
)
