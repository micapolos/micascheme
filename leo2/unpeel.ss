(library (leo2 unpeel)
  (export
    unpeel
    unpeel?)
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

  (define (unpeel? $unpeeled? $term)
    (switch? $term
      (($unpeeled? $unpeeled)
        $unpeeled)
      ((evaluated? $evaluated)
        (unpeel? $unpeeled?
          (evaluated-ref $evaluated)))
      ((labeled? $labeled)
        (unpeel? $unpeeled?
          (labeled-ref $labeled)))))
)
