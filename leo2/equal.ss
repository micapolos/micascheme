(library (leo2 equal)
  (export
    depth-term=?
    term=?)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define-rule-syntax (switch-b? $term ((id? $id) body))
    (switch? $term
      ((nothing? _) #f)
      ((id? $id) body)))

  (define (depth-term=? $depth $term-a $term-b)
    (term-switch $term-a
      ((hole? $hole-a)
        (switch? $term-b
          ((hole? $hole-b)
            (=
              (hole-index $hole-a)
              (hole-index $hole-b)))))
      ((nothing? _) #f)
      ((type? $type-a)
        (switch? $term-b
          ((type? $type-b)
            (=
              (type-depth $type-a)
              (type-depth $type-b)))))
      ((native? $native-a)
        (switch-b? $term-b
          ((native? $native-b)
            (equal?
              (native-ref $native-a)
              (native-ref $native-b)))))
      ((native-application? $native-application-a)
        (switch-b? $term-b
          ((native-application? $native-application-b)
            (and
              (eq?
                (native-application-lambda $native-application-a)
                (native-application-lambda $native-application-b))
              (for-all*
                (partial depth-term=? $depth)
                (native-application-args $native-application-a)
                (native-application-args $native-application-b))))))
      ((variable? $variable-a)
        (switch-b? $term-b
          ((variable? $variable-b)
            (=
              (variable-index $variable-a)
              (variable-index $variable-b)))))
      ((lambda? $lambda-a)
        (switch-b? $term-b
          ((lambda? $lambda-b)
            (depth-term=? (+ $depth 1)
              ($lambda-a (variable $depth))
              ($lambda-b (variable $depth))))))
      ((lambda-type? $lambda-type-a)
        (switch-b? $term-b
          ((lambda-type? $lambda-type-b)
            (and
              (depth-term=? $depth
                (lambda-type-param $lambda-type-a)
                (lambda-type-param $lambda-type-b))
              (depth-term=? $depth
                (lambda-type-lambda $lambda-type-a)
                (lambda-type-lambda $lambda-type-b))))))
      ((application? $application-a)
        (switch-b? $term-b
          ((application? $application-b)
            (and
              (depth-term=? $depth
                (application-lhs $application-a)
                (application-lhs $application-b))
              (depth-term=? $depth
                (application-rhs $application-a)
                (application-rhs $application-b))))))
      ((branch? $branch-a)
        (switch-b? $term-b
          ((branch? $branch-b)
            (and
              (depth-term=? $depth
                (branch-condition $branch-a)
                (branch-condition $branch-b))
              (depth-term=? $depth
                (branch-consequent $branch-a)
                (branch-consequent $branch-b))
              (depth-term=? $depth
                (branch-alternate $branch-a)
                (branch-alternate $branch-b))))))
      ((recursion? $recursion-a)
        (switch-b? $term-b
          ((recursion? $recursion-b)
            (depth-term=? $depth
              (recursion-lambda $recursion-a)
              (recursion-lambda $recursion-b)))))
      ((labeled? $labeled-a)
        (switch-b? $term-b
          ((labeled? $labeled-b)
            (and
              (depth-term=? $depth
                (labeled-label $labeled-a)
                (labeled-label $labeled-b))
              (depth-term=? $depth
                (labeled-ref $labeled-a)
                (labeled-ref $labeled-b))))))
      ((evaluated? $evaluated-a)
        (switch-b? $term-b
          ((evaluated? $evaluated-b)
            (depth-term=? $depth
              (evaluated-ref $evaluated-a)
              (evaluated-ref $evaluated-b)))))
      ((typed? $typed-a)
        (switch-b? $term-b
          ((typed? $typed-b)
            (and
              (depth-term=? $depth
                (typed-type $typed-a)
                (typed-type $typed-b))
              (depth-term=? $depth
                (typed-ref $typed-a)
                (typed-ref $typed-b))))))))

  (define (term=? $term-a $term-b)
    (depth-term=? 0 $term-a $term-b))
)
