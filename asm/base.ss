(library (asm base)
  (export
    define-type
    define-annotated
    define-monoid
    define-monoidical
    define-doable)
  (import (micascheme) (keyword))
  (export
    (import
      (except (micascheme)
        environment
        environment?)
      (syntax lookup)))

  ; for documentation purposes only
  (define-rule-syntax (define-type id ...) (begin))
  (define-rule-syntax (defines id ...) (begin))
  (define-rule-syntax (expects id ...) (begin))

  (defines doable doable?)
  (define-case-syntax (define-doable (doable done))
    (lets
      ($doable? (identifier-append #'doable #'doable #'?))
      ($done? (identifier-append #'done #'done #'?))
      ($pure-done (identifier-append #'doable #'pure #'- #'done))
      #`(begin
        (define (#,$doable? $obj)
          ((or? #,$doable? #,$done?) $obj))
        (define (doable $obj)
          (switch $obj
            ((#,$done? $done) $done)
            ((else $other) (#,$pure-done $other)))))))

  (defines
    annotated
    annotated?
    annotated-annotation
    annotated-ref
    annotated-with-annotation
    annotated-with-ref
    annotated-map
    annotated-update-annotation
    map-annotated)
  (define-case-syntax (define-annotated (annotated annotation))
    (lets
      ($annotated-annotation (identifier-append #'annotated #'annotated #'- #'annotation))
      ($annotated-with-annotation (identifier-append #'annotated #'annotated #'- #'with #'- #'annotation))
      ($annotated-ref (identifier-append #'annotated #'annotated #'- #'ref))
      ($annotated-with-ref (identifier-append #'annotated #'annotated #'- #'with #'- #'ref))
      ($annotated-map (identifier-append #'annotated #'annotated #'- #'map))
      ($map-annotated (identifier-append #'annotated #'map #'- #'annotated))
      ($annotated-update-annotation (identifier-append #'annotated #'annotated #'- #'update #'- #'annotation))
      #`(begin
        (data (annotated annotation ref))
        (define (#,$annotated-map $annotated $proc)
          (#,$annotated-with-ref $annotated
            ($proc (#,$annotated-ref $annotated))))
        (define (#,$map-annotated $proc $annotated)
          (#,$annotated-map $annotated $proc))
        (define (#,$annotated-update-annotation $annotated $proc)
          (#,$annotated-with-annotation $annotated
            ($proc (#,$annotated-annotation $annotated)))))))

  (defines
    empty-monoid
    monoid-append
    list->monoid)
  (define-case-syntax (define-monoid (monoid zero op))
    (lets
      ($empty-monoid (identifier-append #'monoid #'empty #'- #'monoid))
      #`(begin
        (data (monoid ref))
        (define (#,$empty-monoid) zero)
        (define-list->/append (monoid $list) (apply op $list)))))

  (expects
    empty-monoid
    monoid-append)
  (defines
    monoidical
    monoidical?
    monoidical-monoid
    monoidical-ref
    monoidical-with-monoid
    monoidical-with-ref
    pure-monoidical
    monoidical-map
    monoidical-update-monoid
    monoidical-append
    list->monoidical
    map-monoidical)
  (define-case-syntax (define-monoidical (monoidical monoid))
    (lets
      ($monoidical-monoid (identifier-append #'monoidical #'monoidical #'- #'monoid))
      ($monoidical-ref (identifier-append #'monoidical #'monoidical #'- #'ref))
      ($pure-monoidical (identifier-append #'monoidical #'pure #'- #'monoidical))
      ($empty-monoid (identifier-append #'monoidical #'empty #'- #'monoid))
      ($monoid-append (identifier-append #'monoidical #'monoid #'- #'append))
      #`(begin
        (define-annotated (monoidical monoid))
        (define (#,$pure-monoidical $ref)
          (monoidical (#,$empty-monoid) $ref))
        (define-list->/append (monoidical $list)
          (monoidical
            (apply #,$monoid-append (map #,$monoidical-monoid $list))
            (map #,$monoidical-ref $list))))))
)
