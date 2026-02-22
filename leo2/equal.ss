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
      ((anything? _) #t)
      ((id? $id) body)))

  (define (depth-term=? $depth $term-a $term-b)
    (term-switch $term-a
      ((nothing? _)
        (anything? $term-b))
      ((anything? _)
        #t)
      ((type? $type-a)
        (switch? $term-b
          ((type? $type-b)
            (=
              (type-depth $type-a)
              (type-depth $type-b)))))
      ((symbol? $symbol-a)
        (switch-b? $term-b
          ((symbol? $symbol-b)
            (symbol=?
              $symbol-a
              $symbol-b))))
      ((indexed? $indexed-a)
        (switch-b? $term-b
          ((indexed? $indexed-b)
            (and
              (=
                (indexed-index $indexed-a)
                (indexed-index $indexed-b))
              (depth-term=? $depth
                (indexed-ref $indexed-a)
                (indexed-ref $indexed-b))))))
      ((symbolic? $symbolic-a)
        (switch-b? $term-b
          ((symbolic? $symbolic-b)
            (and
              (symbol=?
                (symbolic-symbol $symbolic-a)
                (symbolic-symbol $symbolic-b))
              (depth-term=? $depth
                (symbolic-ref $symbolic-a)
                (symbolic-ref $symbolic-b))))))
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
                (native-application-procedure $native-application-a)
                (native-application-procedure $native-application-b))
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
      ((procedure? $procedure-a)
        (switch-b? $term-b
          ((procedure? $procedure-b)
            (depth-term=? (+ $depth 1)
              ($procedure-a (variable $depth))
              ($procedure-b (variable $depth))))))
      ((signature? $signature-a)
        (switch-b? $term-b
          ((signature? $signature-b)
            (and
              (depth-term=? $depth
                (signature-param $signature-a)
                (signature-param $signature-b))
              (depth-term=? $depth
                (signature-procedure $signature-a)
                (signature-procedure $signature-b))))))
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
              (recursion-procedure $recursion-a)
              (recursion-procedure $recursion-b)))))
      ((annotated? $annotated-a)
        (switch-b? $term-b
          ((annotated? $annotated-b)
            (and
              (depth-term=? $depth
                (annotated-annotation $annotated-a)
                (annotated-annotation $annotated-b))
              (depth-term=? $depth
                (annotated-ref $annotated-a)
                (annotated-ref $annotated-b))))))
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
