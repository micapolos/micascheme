(library (syntax lookup)
  (export
    empty-lookup
    lookup+
    lookup-undefined-id
    lookup+undefined
    lookup-gen
    lookup-ref
    lookup-transformer
    lookup-transform
    lookup-unbound
    lookup-gen?
    lookup-with
    list->lookup
    lookup-append)
  (import (micascheme))

  (define lookup-gen?
    (make-parameter #t))

  (define (empty-lookup)
    (lambda (_) #f))

  (define (lookup-ref $lookup $id)
    (or
      ($lookup $id)
      (lookup-unbound $id)))

  (define (lookup+ $lookup $id $item)
    (lambda ($lookup-id)
      (if (free-identifier=? $lookup-id $id)
        $item
        ($lookup $lookup-id))))

  (define (lookup-undefined-id $lookup $id)
    (if ($lookup $id)
      (syntax-error $id "already defined")
      $id))

  (define (lookup+undefined $lookup $id $item)
    (lookup+ $lookup (lookup-undefined-id $lookup $id) $item))

  (define (lookup-gen $lookup $id)
    (lets
      ($identifier
        (if (lookup-gen?)
          (generate-identifier $id)
          $id))
      (pair
        (lookup+ $lookup $id $identifier)
        $identifier)))

  (define (lookup-transformer $lookup $id)
    (switch ($lookup $id)
      ((identifier? _) #f)
      ((false? _) #f)
      ((else $transformer) $transformer)))

  (define (lookup-transform $lookup $transformer $syntax)
    (transform $transformer $syntax $lookup))

  (define (lookup-unbound $id)
    (syntax-error $id "undefined"))

  (define-rule-syntax (lookup-with (id item) ...)
    (fluent (empty-lookup)
      (lookup+ #'id item) ...))

  (define-list->/append (lookup $lookups)
    (lambda ($id)
      (map-find
        (lambda ($lookup) ($lookup $id))
        $lookups)))
)
