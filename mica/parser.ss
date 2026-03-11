(library (mica parser)
  (export
    parse-string
    eof
    char string
    the
    prefixed suffixed
    check-parses
    check-parse-error)
  (import
    (except (micascheme) string eof)
    (getter))

  (define (parse-string $parser $string)
    (lets
      ((values $value $bfp $line $column)
        (getter-get!
          (ending-getter (getter-item-getter $parser) eof-getter)
          (open-input-string $string)
          test-sfd
          0 0 0 0))
      $value))

  (define eof
    (getter-item (lambda (_) #f) eof-getter))

  (define char
    (getter-item char? char-getter))

  (define string
    (getter-item char? string-getter))

  (define-rules-syntax
    ((the ch)
      (char? (datum ch))
      (getter-item
        (partial char=? (datum ch))
        (exact-char-getter (datum ch))))
    ((the s)
      (string? (datum s))
      (if (string-empty? (datum s))
        (syntax-error s "empty string")
        (getter-item
          (partial char=? (string-ref (datum s) 0))
          (exact-string-getter (datum s)))))
    ((the x)
      x))

  (define-rule-syntax (prefixed $prefix $item)
    (getter-item
      (getter-item-first-char? (the $prefix))
      (starting-getter
        (getter-item-getter (the $prefix))
        (getter-item-getter (the $item)))))

  (define-rule-syntax (suffixed $item $suffix)
    (getter-item
      (getter-item-first-char? (the $item))
      (ending-getter
        (getter-item-getter (the $item))
        (getter-item-getter (the $suffix)))))

  (define-rule-syntax (check-parses parser in out)
    (check (datum/annotation=? (parse-string (the parser) in) out)))

  (define-rule-syntax (check-parse-error parser in)
    (check (raises (parse-string (the parser) in))))
)
