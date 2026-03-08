(library (reader)
  (export
    reader-get!

    reader
    reader-bind
    reader-map
    reader-lets
    list->reader
    append-reader
    apply-reader

    next-reader
    push-reader
    list-reader

    check-reader-gets!)
  (import
    (scheme)
    (monadic)
    (lets)
    (switch)
    (eof)
    (stack)
    (check)
    (syntax))

  ; === monad ===

  (define (reader-get! $reader $read!)
    ($reader $read!))

  (define (reader $value)
    (lambda ($read!) $value))

  (define (reader-bind $reader $fn)
    (lambda ($read!)
      (lets
        ($value (reader-get! $reader $read!))
        (reader-get! ($fn $value) $read!))))

  (define-monadic reader)

  ; === utilities ===

  (define next-reader
    (lambda ($read!) ($read!)))

  (define (push-reader $stack $reader)
    (reader-lets
      ($value/eof next-reader)
      (switch $value/eof
        ((eof? $eof)
          (reader $stack))
        ((else $value)
          (push-reader (push $stack $value) $reader)))))

  (define (list-reader $reader)
    (reader-map reverse (push-reader (stack) $reader)))

  ; === checks ===

  (define-rule-syntax (check-reader-gets! reader string out)
    (check (equal? (reader-get! reader (string->read! string)) out)))
)
