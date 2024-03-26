(library (identifier)
  (export
    identifier-named?
    build-identifier)
  (import
    (scheme)
    (syntax))

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
)
