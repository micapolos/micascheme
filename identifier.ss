(library (identifier)
  (export
    identifier-named?
    build-identifier
    identifier-append)
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
)
