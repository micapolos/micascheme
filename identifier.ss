(library (identifier)
  (export
    identifier-named?
    build-identifier
    identifier-append
    datum?
    keywords
    identifier)
  (import
    (scheme)
    (syntax)
    (symbol))

  (define-rule-syntax (identifier-named? $syntax $name)
    (and
      (identifier? $syntax)
      (symbol=? (syntax->datum $syntax) (quote $name))))

  (define-rule-syntax (build-identifier ($var $id) $body)
    (datum->syntax $id
      (string->symbol
        (let
          (($var (symbol->string (syntax->datum $id))))
          $body))))

  (define (identifier-append $tpl . $ids)
    (datum->syntax $tpl
      (apply symbol-append (map syntax->datum $ids))))

  (define-rule-syntax (datum? x ...)
    (equal?
      (syntax->datum #'(x ...))
      '(x ...)))

  (define-rule-syntax (keywords id ...)
    (and
      (or
        (datum? id)
        (syntax-error #'id
          (format "expected ~a instead of" 'id))) ...))

  (define-rule-syntax (identifier id)
    (if (identifier? #'id)
      #'id
      (syntax-error #'id "not identifier")))
)
