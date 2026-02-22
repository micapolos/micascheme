(library (leo2 datum)
  (export
    term->datum
    depth-term->datum
    check-term->datum=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol))

  (define (term->datum $term)
    (depth-term->datum 0 $term))

  (define (depth-term->datum $depth $term)
    (term-switch $term
      ((nothing? _) 'nothing)
      ((anything? _) 'anything)
      ((type? $type)
        `(type ,(type-depth $type)))
      ((symbol? $symbol)
        `(symbol ,$symbol))
      ((indexed? $indexed)
        `(indexed
          ,(indexed-index $indexed)
          ,(depth-term->datum $depth (indexed-ref $indexed))))
      ((symbolic? $symbolic)
        `(symbolic
          ,(symbolic-symbol $symbolic)
          ,(depth-term->datum $depth (symbolic-ref $symbolic))))
      ((native? $native)
        `(native ,(native-ref $native)))
      ((native-application? $native-application)
        `(native-apply
          ,(native-application-procedure $native-application)
          ,@(map
            (partial depth-term->datum $depth)
            (native-application-args $native-application))))
      ((variable? $variable)
        `(the ,(variable-symbol $variable)))
      ((abstraction? $abstraction)
        `(lambda
          . ,(procedure->datum $depth #f
            (abstraction-procedure $abstraction))))
      ((signature? $signature)
        `(lambda
          . ,(procedure->datum $depth
            (signature-param $signature)
            (signature-procedure $signature))))
      ((application? $application)
        `(apply
          ,(depth-term->datum $depth (application-lhs $application))
          ,(depth-term->datum $depth (application-rhs $application))))
      ((branch? $branch)
        `(if
          ,(depth-term->datum $depth (branch-condition $branch))
          ,(depth-term->datum $depth (branch-consequent $branch))
          ,(depth-term->datum $depth (branch-alternate $branch))))
      ((recursion? $recursion)
        `(recursive .
          ,(procedure->datum $depth #f
            (recursion-procedure $recursion))))
      ((annotated? $annotated)
        `(annotated
          ,(depth-term->datum $depth
            (annotated-annotation $annotated))
          ,(depth-term->datum $depth
            (annotated-ref $annotated))))
      ((evaluated? $evaluated)
        `(evaluated
          ,(depth-term->datum $depth
            (evaluated-ref $evaluated))))
      ((typed? $typed)
        (lets
          ($ref-datum
            (depth-term->datum $depth
              (typed-ref $typed)))
          `(typed
            ,(depth-term->datum $depth (typed-type $typed))
            ,$ref-datum)))))

  (define (procedure->datum $depth $param? $procedure)
    (lets
      ($symbol (depth->symbol $depth))
      `(
        ,(switch $param?
          ((false? _)
            $symbol)
          ((else $param)
            `(,$symbol ,(depth-term->datum $depth $param))))
        ,(depth-term->datum
          (+ $depth 1)
          (app $procedure (variable $symbol))))))

  (define-rule-syntax (check-term->datum=? in out)
    (check (equal? (term->datum in) `out)))
)
