(library (leo2 unpeel)
  (export unpeel)
  (import
    (leo2 base)
    (leo2 term))

  (define (unpeel $term)
    (switch $term
      ((evaluated? $evaluated)
        (unpeel (evaluated-ref $evaluated)))
      ((labeled? $labeled)
        (unpeel (labeled-ref $labeled)))
      ((typed? $typed)
        (unpeel (typed-ref $typed)))
      ((else $unpeeled)
        $unpeeled)))
)
