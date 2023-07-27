(library (parser)
  (export
    parser parser? parser-value parser-push-fn parser!

    parser-push
    parser-push-string-from parser-push-string
    parse

    parser-bind

    the
    char)

  (import (micascheme))

  (data (parser value push-fn))

  (define-syntax-rule (parser! value (var body))
    (parser value (lambda (var) body)))

  (define (parser-push $parser $char)
    ((parser-push-fn $parser) $char))

  (define (parser-push-string-from $parser $string $index)
    (cond
      ((>= $index (string-length $string)) $parser)
      (else 
        (switch (parser-push $parser (string-ref $string $index))
          ((failure? $failure) (failure (indexed (failure-value $failure) $index)))
          ((parser? $parser) (parser-push-string-from $parser $string (+ $index 1)))))))

  (define (parser-push-string $parser $string)
    (parser-push-string-from $parser $string 0))

  (define (parse $parser $string)
    (switch (parser-push-string $parser $string)
      ((parser? $parser) (parser-value $parser))
      ((failure? $failure) $failure)))

  ; --- combinators

  (define (parser-bind $parser $fn)
    (switch (parser-value $parser)
      ((eof-object? $eof-object)
        (parser $eof-object
          (lambda ($char)
            (switch (parser-push $parser $char)
              ((failure? $failure) $failure)
              ((parser? $parser) (parser-bind $parser $fn))))))
      ((else $value) 
        (lets
          ($fn-parser ($fn $value))
          (parser (parser-value $fn-parser)
            (lambda ($char)
              (switch (parser-push $parser $char)
                ((failure? $failure) (parser-push $fn-parser $char))
                ((parser? $parser) (parser-bind $parser $fn)))))))))

  ; --- DSL

  (define (the $value)
    (parser! $value ($char (failure $char))))

  (define char
    (parser! (eof-object) ($char (the $char))))
)