(library (parser)
  (export
    parser parser? parser-value parser-push-fn parser!

    parser-push
    parser-push-string-from parser-push-string
    parse

    parser-bind

    the
    char
    single-char

    make-parser
    define-parser)

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

  (define (value-parser $value)
    (parser! $value ($char (failure $char))))

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

  (define (single-char $char)
    (parser (void)
      (lambda ($new-char)
        (cond 
          ((char=? $new-char $char) 
            (parser (void) 
              (lambda ($new-char) 
                (failure (eof-object)))))
          (else (failure `(single ,$char)))))))

  (define-syntax make-parser
    (lambda ($syntax)
      (syntax-case $syntax (lets)
        ((_ (lets $binding ... $body))
          (fold-right 
            (lambda ($binding $parser)
              (syntax-case $binding ()
                (($var $body)
                  #`(parser-bind (make-parser $body)
                    (lambda ($var) #,$parser)))))
            #`(make-parser $body)
            (syntax->list #`($binding ...))))
        ((_ $body) 
          (switch (syntax->datum #`$body)
            ((char? $char) #`(single-char #,$char))
            ((string? $string) (failure "todo"))
            ((else $other) #`$body))))))

  (define-syntax define-parser
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $id $body)
          #`(define $id
            (make-parser $body))))))
)
