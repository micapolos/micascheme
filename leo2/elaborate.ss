(library (leo2 elaborate)
  (export
    elaborate
    check-elaborates)
  (import
    (leo2 base)
    (leo2 term)
    (leo2 datum))

  (define (elaborate $term)
    (term-switch $term
      ((nothing? _) #f)
      ((anything? _) #t)
      ((type? $type) $type)
      ((native? $native) (todo))
      ((native-application? $native-application) (todo))
      ((variable? $variable) (todo))
      ((procedure? $procedure) (todo))
      ((signature? $signature) (todo))
      ((application? $application) (todo))
      ((branch? $branch) (todo))
      ((recursion? $recursion) (todo))
      ((labeled? $labeled) (todo))
      ((evaluated? $evaluated) (todo))
      ((typed? $typed)
        (type-elaborate
          (typed-type $typed)
          (typed-ref $typed)))))

  (define (type-elaborate $type $term)
    (term-switch $term
      ((nothing? _) #f)
      ((anything? _) #t)
      ((type? $type) $type)
      ((native? $native) (todo))
      ((native-application? $native-application) (todo))
      ((variable? $variable) (todo))
      ((procedure? $procedure) (todo))
      ((signature? $signature) (todo))
      ((application? $application) (todo))
      ((branch? $branch) (todo))
      ((recursion? $recursion) (todo))
      ((labeled? $labeled) (todo))
      ((evaluated? $evaluated) (todo))
      ((typed? $typed)
        (type-elaborate
          (typed-type $typed)
          (typed-ref $typed)))))

  (define-rule-syntax (check-elaborates in out)
    (check-term->datum=? (elaborate in) out))
)
