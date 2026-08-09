(library (tt keywords)
  (export
    forall lambda pi quote
    product type
    boolean number char string datum
    tuple choice
    typeof
    if
    is?
    time
    unchecked
    macro
    ...
    tuple-constructor
    tuple-accessor
    choice-constructor
    choice-matcher)
  (import
    (prefix (scheme) %)
    (syntax))

  (%define-syntax boolean (%make-compile-time-value #t))
  (%define-syntax number (%make-compile-time-value #t))
  (%define-syntax string (%make-compile-time-value #t))
  (%define-syntax char (%make-compile-time-value #t))
  (%define-syntax datum (%make-compile-time-value #t))
  (%define-syntax tuple (%make-compile-time-value #t))
  (%define-syntax choice (%make-compile-time-value #t))

  (define-keywords
    forall lambda pi quote
    product type
    if
    unchecked
    macro
    typeof
    is?
    time
    ...
    tuple-constructor
    tuple-accessor
    choice-constructor
    choice-matcher)
)
