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
      ((symbol? $symbol)
        (typed
          (typed (type 0) $symbol)
          $symbol))
      ((indexed? $indexed)
        (lets
          ($typed-ref (elaborate (indexed-ref $indexed)))
          (typed
            (typed (type 0)
              (indexed
                (indexed-index $indexed)
                (typed-type $typed-ref)))
            (typed-ref $typed-ref))))
      ((symbolic? $symbolic) (todo))
      ((native? $native) (todo))
      ((native-application? $native-application) (todo))
      ((variable? $variable) (todo))
      ((abstraction? $abstraction) (todo))
      ((signature? $signature) (todo))
      ((application? $application) (todo))
      ((branch? $branch) (todo))
      ((recursion? $recursion) (todo))
      ((annotated? $annotated) (todo))
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
      ((symbol? $symbol)
        (typed
          (typed (type 0) $symbol)
          $symbol))
      ((indexed? $indexed)
        (lets
          ($typed-ref (elaborate (indexed-ref $indexed)))
          (typed
            (typed (type 0)
              (indexed
                (indexed-index $indexed)
                (typed-type $typed-ref)))
            (typed-ref $typed-ref))))
      ((symbolic? $symbolic) (todo))
      ((native? $native) (todo))
      ((native-application? $native-application) (todo))
      ((variable? $variable) (todo))
      ((abstraction? $abstraction) (todo))
      ((signature? $signature) (todo))
      ((application? $application) (todo))
      ((branch? $branch) (todo))
      ((recursion? $recursion) (todo))
      ((annotated? $annotated) (todo))
      ((evaluated? $evaluated) (todo))
      ((typed? $typed)
        (type-elaborate
          (typed-type $typed)
          (typed-ref $typed)))))

  (define-rule-syntax (check-elaborates in out)
    (check-term->datum=? (elaborate in) out))
)
