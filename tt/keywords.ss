(library (tt keywords)
  (export
    forall lambda pi quote
    boolean number char string datum
    tuple tuple-ref
    union union-case
    if
    unchecked
    macro
    ...
    tuple-constructor
    tuple-accessor)
  (import
    (prefix (scheme) %)
    (syntax))

  (%define-syntax boolean (%make-compile-time-value #t))
  (%define-syntax number (%make-compile-time-value #t))
  (%define-syntax string (%make-compile-time-value #t))
  (%define-syntax char (%make-compile-time-value #t))
  (%define-syntax datum (%make-compile-time-value #t))
  (%define-syntax tuple (%make-compile-time-value #t))
  (%define-syntax union (%make-compile-time-value #t))

  (define-keywords
    forall lambda pi quote
    tuple-ref
    union-case
    if
    unchecked
    macro
    ...
    tuple-constructor
    tuple-accessor)
)
