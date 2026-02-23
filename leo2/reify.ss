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

  (define (labeled-datum? $labeled $symbol)
    (switch? (labeled-label $labeled)
      ((quoted? $quoted)
        (lets
          ($ref (quoted-ref $quoted))
          (and
            (pair? $ref)
            (eq? (car $ref) $symbol)
            (pair? (cdr $ref))
            (cadr $ref))))))

  (define (depth-reify $depth $native-datum? $term)
    (term-switch $term
      ((nothing? $nothing)
        (throw reify $nothing))
      ((anything? $anything)
        (throw reify $anything))
      ((type? $type)
        (throw reify $type))
      ((quoted? $quoted)
        (throw reify $quoted))
      ((native? $native)
        (or $native-datum? (throw reify $native)))
      ((native-application? $native-application)
        (or
          (and
            $native-datum?
            `(,$native-datum?
              ,@(map
                (partial depth-reify $depth #f)
                (native-application-args $native-application))))
          (throw reify $native-application)))
      ((variable? $variable)
        (depth->symbol (variable-index $variable)))
      ((procedure? $procedure)
        (lets
          ($symbol (depth->symbol $depth))
          `(lambda (,$symbol)
            ,(depth-reify (+ $depth 1) #f
              ($procedure (variable $depth))))))
      ((signature? $signature)
        (throw reify $signature))
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
          (or (labeled-datum? $labeled 'native) $native-datum?)
          (labeled-ref $labeled)))
      ((evaluated? $evaluated)
        (throw reify $evaluated))
      ((typed? $typed)
        (throw reify $typed))))

  (define-rule-syntax (check-reify in out)
    (check (equal? (reify in) 'out)))
)
