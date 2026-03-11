(library (mica parser)
  (export
    parse-string
    eof
    char ?char
    string
    alphabetic-string
    numeric-string
    digit
    the
    prefixed suffixed
    indented
    optional
    map
    list
    one-of
    check-parses
    check-parse-error)
  (import
    (rename
      (except (micascheme) map eof list)
      (string %string))
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

  (define (?char $test?)
    (getter-item $test? (test?-char-getter $test?)))

  (define string
    (getter-item char? string-getter))

  (define alphabetic-string
    (getter-item char-alphabetic? alphabetic-string-getter))

  (define numeric-string
    (getter-item char-numeric? numeric-string-getter))

  (define-rules-syntaxes (keywords else)
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
    ((the x) x)
    ((prefixed $prefix $item)
      (getter-item
        (getter-item-first-char? (the $prefix))
        (starting-getter
          (getter-item-getter (the $prefix))
          (getter-item-getter (the $item)))))
    ((suffixed $item $suffix)
      (getter-item
        (getter-item-first-char? (the $item))
        (ending-getter
          (getter-item-getter (the $item))
          (getter-item-getter (the $suffix)))))
    ((indented $item)
      (getter-item
        (getter-item-first-char? (the $item))
        (indented-getter (getter-item-getter (the $item)))))
    ((optional $item)
      (getter-item
        (getter-item-first-char? $item)
        (optional-getter
          (getter-item-first-char? (the $item))
          (getter-item-getter (the $item)))))
    ((one-of first ... last)
      (getter-item
        (or?
          (getter-item-first-char? first)
          ...
          (getter-item-first-char? last))
        (getter-switch peek-char/eof-getter
          (((getter-item-first-char? (the first)) _)
            (getter-item-getter (the first)))
          ...
          ((else _)
            (getter-item-getter (the last))))))
    ((map item fn)
      (getter-item
        (getter-item-first-char? (the item))
        (getter-map (getter-item-getter (the item)) fn)))
    ((map item fn fns ...)
      (map (map item fn) fns ...))
    ((list item)
      (lets
        ($item (the item))
        ($first-char? (getter-item-first-char? (the item)))
        (getter-item
          $first-char?
          (eol?-list-getter
            (not? $first-char?)
            (getter-item-getter $item))))))

  (define digit
    (map (?char char-numeric?) %string string->number))

  (define-rule-syntax (check-parses parser in out)
    (check (datum/annotation=? (parse-string (the parser) in) out)))

  (define-rule-syntax (check-parse-error parser in)
    (check (raises (parse-string (the parser) in))))
)
