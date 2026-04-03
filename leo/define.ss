(library (leo define)
  (export define value union type)
  (import
    (rename
      (except (scheme) predicate)
      (define %define)
      (lambda %lambda))
    (only (chezscheme) define-property)
    (rename (data) (data %data))
    (rename (union) (union %union))
    (leo lambda)
    (syntax-keywords)
    (keyword)
    (syntax)
    (syntaxes)
    (leo record)
    (leo maker)
    (leo predicate)
    (leo getter-leo)
    (leo setter!))

  (define-keywords value union type)

  (define-syntax (define-custom $syntax)
    (%lambda ($lookup)
      (syntax-case $syntax ()
        ((_ (id . x))
          (and
            (keyword? id)
            (guard
              (_ (else #f))
              ($lookup #'id #'definer)))
          ($lookup #'id #'definer))
        ((_ (id x))
          (keyword? id)
          #'(%define id x)))))

  (define-rules-syntaxes
    ; (todo define all of these using definer)
    (keywords getter setter! maker predicate value lambda syntax and when keywords record type union)

    ((define-1 (value (id x)))
      (%define id x))

    ((define-1 (lambda (id x xs ...)))
      (%define (id) x xs ...))

    ((define-1 (lambda (id param ... (and last)) x xs ...))
      (%define (id param ... . last) x xs ...))

    ((define-1 (lambda (id param ...) x xs ...))
      (%define (id param ...) x xs ...))

    ((define-1 (syntax (keywords k ...) (when pattern x xs ...) ...))
      (define-rules-syntaxes (keywords k ...)
        (pattern x xs ...) ...))

    ((define-1 (syntax (when pattern x xs ...) ...))
      (define (syntax (keywords) (when pattern x xs ...) ...)))

    ((define-1 (syntax (id x)))
      (keyword? id)
      (define-syntax id x))

    ((define-1 (syntax (id s) x xs ...))
      (keyword? id)
      (define-syntax (id s) x xs ...))

    ((define-1 (getter (id proc)))
      (define-property id getter proc))

    ((define-1 (setter! (id proc)))
      (define-property id setter! proc))

    ((define-1 (maker (id proc)))
      (define-property id maker proc))

    ((define-1 (predicate (id proc)))
      (define-property id predicate proc))

    ((define-1 (record (type (id . x))))
      (keyword? id)
      (define-record-type id . x))

    ((define-1 (record (type . x)))
      (define-record-type . x))

    ((define-1 (record . x))
      (%data . x))

    ((define-1 (union . x))
      (%union . x))

    ((define-1 . x)
      (define-custom . x))

    ((define x ...)
      (begin
        (define-1 x)
        ...)))
)
