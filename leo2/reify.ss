(library (leo2 reify)
  (export
    reify
    check-reify)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 symbol)
    (leo2 datum))

  (define (reify $term)
    (depth-reify 0 #f $term))

  (define (depth-reify $depth $native? $term)
    (term-switch $term
      ((nothing? $nothing) #f)
      ((anything? $anything) #f)
      ((type? $type) #f)
      ((native? $native)
        (switch $native?
          ((native? $native) (native-ref $native))
          ((else _)
            (switch (native-ref $native)
              ((literal? $literal) $literal)
              ((else _) (throw reify $native))))))
      ((native-application? $native-application)
        (switch $native?
          ((native? $native)
            `(,(native-ref $native)
              ,@(map
                (partial depth-reify $depth #f)
                (native-application-args $native-application))))
          ((else _)
            (throw reify $native-application))))
      ((variable? $variable)
        (depth->symbol (variable-index $variable)))
      ((procedure? $procedure)
        (lets
          ($symbol (depth->symbol $depth))
          `(lambda (,$symbol)
            ,(depth-reify (+ $depth 1) #f
              ($procedure (variable $depth))))))
      ((signature? $signature) #f)
      ((application? $application)
        `(
          ,(depth-reify $depth #f (application-lhs $application))
          ,(depth-reify $depth #f (application-rhs $application))))
      ((branch? $branch)
        `(if
          ,(depth-reify $depth #f (branch-condition $branch))
          ,(depth-reify $depth #f (branch-consequent $branch))
          ,(depth-reify $depth #f (branch-alternate $branch))))
      ((recursion? $recursion)
        (lets
          ($symbol (depth->symbol $depth))
          `(letrec
            ((
              ,$symbol
              ,(depth-reify (+ $depth 1) #f
                 (recursion-apply $recursion (variable $depth)))))
            ,$symbol)))
      ((labeled? $labeled)
        (depth-reify
          $depth
          (or
            (switch? (labeled-label $labeled)
              ((native? $native) $native))
            $native?)
          (labeled-ref $labeled)))
      ((evaluated? $evaluated)
        (depth-reify $depth (evaluated-ref $evaluated)))
      ((typed? $typed)
        (depth-reify $depth (typed-ref $typed)))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify in) 'out)))
)
