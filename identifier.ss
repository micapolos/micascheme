(library (identifier)
  (export
    identifier-named?
    build-identifier)
  (import
    (scheme)
    (syntax))

  (define-syntax-rule (identifier-named? $syntax $name)
    (and
      (identifier? $syntax)
      (symbol=? (syntax->datum $syntax) (quote $name))))

  (define-syntax-rule (build-identifier ($var $id) $body)
    (datum->syntax $id
      (string->symbol
        (let
          (($var (symbol->string (syntax->datum $id))))
          $body))))
)
