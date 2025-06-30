(library (typico core fragments)
  (export
    + - string-append string-length
    boolean-type integer-type char-type string-type)
  (import
    (prefix (typico base) %)
    (typico fragment))

  (%define-rule-syntax (define-fragments imp id %...)
    (%begin
      (%define id (fragment (%import imp) id)) %...))

  (define-fragments (scheme)
    + -
    string-append
    string-length)

  (define-fragments (typico type)
    boolean-type integer-type char-type string-type)
)
