(library (leo2 datum)
  (export
    term->datum
    depth-term->datum
    check-term->datum=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol)
    (procedure-name))

  (define (term->datum $term)
    (depth-term->datum 0 $term))

  (define (native->datum $ref)
    (switch $ref
      ((procedure? $procedure)
        (or
          (procedure-name? $procedure)
          $procedure))
      ((symbol? $symbol)
        `',$symbol)
      ((literal? $literal)
        $literal)
      ((record? $record)
        (record-type-name (record-rtd $record)))
      ((else $other)
        `',$other)))

  (define (depth-term->datum $depth $term)
    (term-switch $term
      ((hole? $hole) `(hole ,(hole-index $hole)))
      ((nothing? _) 'nothing)
      ((type? $type)
        `(type ,(type-depth $type)))
      ((native? $native)
        `(native ,(native->datum (native-ref $native))))
      ((native-application? $native-application)
        `(native-application
          ,(native->datum (native-application-procedure $native-application))
          (list
            ,@(map
              (partial depth-term->datum $depth)
              (native-application-args $native-application)))))
      ((variable? $variable)
        `(variable ,(variable-index $variable)))
      ((procedure? $procedure)
        (lets
          ($symbol (depth->symbol $depth))
          `(lambda
            ,(depth-term->datum
              (+ $depth 1)
              ($procedure (variable $depth))))))
      ((signature? $signature)
        `(signature
          ,(depth-term->datum $depth (signature-param $signature))
          ,(depth-term->datum $depth (signature-procedure $signature))))
      ((application? $application)
        `(application
          ,(depth-term->datum $depth (application-lhs $application))
          ,(depth-term->datum $depth (application-rhs $application))))
      ((branch? $branch)
        `(branch
          ,(depth-term->datum $depth (branch-condition $branch))
          ,(depth-term->datum $depth (branch-consequent $branch))
          ,(depth-term->datum $depth (branch-alternate $branch))))
      ((recursion? $recursion)
        `(recursion
          ,(depth-term->datum $depth (recursion-procedure $recursion))))
      ((labeled? $labeled)
        `(labeled
          ,(depth-term->datum $depth (labeled-label $labeled))
          ,(depth-term->datum $depth (labeled-ref $labeled))))
      ((evaluated? $evaluated)
        `(evaluated
          ,(depth-term->datum $depth (evaluated-ref $evaluated))))
      ((typed? $typed)
        `(typed
          ,(depth-term->datum $depth (typed-type $typed))
          ,(depth-term->datum $depth (typed-ref $typed))))))

  (define-rule-syntax (check-term->datum=? in out)
    (check (equal? (term->datum in) `out)))
)
