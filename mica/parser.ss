(library (mica parser)
  (export
    parse-string
    return
    or
    eof
    char ?char
    category-char
    range-char
    char>=
    char<=
    whitespace-char
    alphabetic-char
    numeric-char
    string
    null
    alphabetic-string
    numeric-string
    digit
    the
    prefixed suffixed wrapped
    non-empty-separated
    separated
    indented
    optional
    map
    apply
    string->datum
    list non-empty-list
    prepend
    append
    string
    string-append
    list->string
    first-char
    not
    >
    list-string
    one-of
    check-parses
    check-parse-error
    check-parser ok error
    annotation
    switch
    else)
  (import
    (rename (except (micascheme) map eof list switch error)
      (string %string)
      (prepend %prepend)
      (append %append)
      (string-append %string-append)
      (list->string %list->string)
      (apply %apply)
      (not %not)
      (> %>)
      (char %char)
      (else %else)
      (or %or)
      (null %null))
    (getter))

  (define-keywords not > else ok error)

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

  (define (?char $test?)
    (getter-item $test? (test?-char-getter $test?)))

  (define whitespace-char (?char char-whitespace?))
  (define alphabetic-char (?char char-alphabetic?))
  (define numeric-char (?char char-numeric?))

  ;(define string (getter-item char? string-getter))
  (define alphabetic-string
    (getter-item char-alphabetic? alphabetic-string-getter))

  (define numeric-string
    (getter-item char-numeric? numeric-string-getter))

  (define-syntax (string $syntax)
    (syntax-case $syntax ()
      ; TODO: Update apply to allow zero arguments.
      ((_)
        #'(the ""))
      ((_ ch ...)
        #'(apply (%string (the ch) ...)))
      (id
        (identifier? #'id)
        #'(getter-item char? string-getter))))

  (define-syntax (char $syntax)
    (syntax-case $syntax ()
      ((_ ch)
        #'(getter-item
          (partial char=? (%char ch))
          (exact-char-getter (%char ch))))
      (id
        (identifier? #'id)
        #'(getter-item char? char-getter))))

  (define-syntax (null $syntax)
    (syntax-case $syntax ()
      (id (identifier? #'id)
        #'(return %null))))

  (define-rules-syntaxes (keywords else not >)
    ((return x)
      (getter-item
        (always #f)
        (getter x)))
    ((the ch)
      (char? (datum ch))
      (getter-item
        (partial char=? (datum ch))
        (exact-char-getter (datum ch))))
    ((the s)
      (string? (datum s))
      (if (string-empty? (datum s))
        (getter-item
          (lambda (_) #f)
          (exact-string-getter (datum s)))
        (getter-item
          (partial char=? (string-ref (datum s) 0))
          (exact-string-getter (datum s)))))
    ((the x) x)
    ((category-char cat ...)
      (?char
        (lambda ($char)
          (lets
            ($category (char-general-category $char))
            (%or (symbol=? $category 'cat) ...)))))
    ((range-char from to)
      (and (char? (datum from)) (char? (datum to)))
      (?char
        (lambda ($char)
          (and
            (char>=? $char from)
            (char<=? $char to)))))
    ((char>= ch)
      (char? (datum ch))
      (?char (lambda ($char) (char>=? $char ch))))
    ((char<= ch)
      (char? (datum ch))
      (?char (lambda ($char) (char<=? $char ch))))
    ((char-test (> ch))
      (lambda ($char)
        (char>? $char ch)))
    ((char-test (not ch ...))
      (lambda ($char)
        (and (%not (char=? $char ch)) ...)))
    ((string->datum s)
      (map s
        (lambda ($string)
          (read (open-input-string $string)))))
    ((first-char test? ... parser)
      (lets
        ($parser (the parser))
        (getter-item
          (lambda ($char)
            (and
              (app (char-test test?) $char) ...
              ((getter-item-first-char? $parser) $char)))
          (getter-switch peek-char/eof-getter
            ((char? $char)
              (if (and (app (char-test test?) $char) ...)
                (getter-item-getter parser)
                (error-getter "unexpected char" $char)))
            ((%else $other)
              (getter-item-getter parser))))))
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
    ((wrapped $prefix $item $suffix)
      (suffixed (prefixed $prefix $item) $suffix))
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
          (getter-item-first-char? (the first))
          ...
          (getter-item-first-char? (the last)))
        (getter-switch peek-char/eof-getter
          (((getter-item-first-char? (the first)) _)
            (getter-item-getter (the first)))
          ...
          ((%else _)
            (getter-item-getter (the last))))))
    ((map item fn)
      (getter-item
        (getter-item-first-char? (the item))
        (getter-map (getter-item-getter (the item)) fn)))
    ((map item fn fns ...)
      (map (map item fn) fns ...))
    ((apply (fn item items ...))
      (getter-item
        (getter-item-first-char? (the item))
        (apply-getter fn
          (getter-item-getter (the item))
          (getter-item-getter (the items)) ...)))
    ((prepend item list)
      (apply (%prepend (the item) (the list))))
    ((append item ...)
      (apply (%append (the item) ...)))
    ((list item)
      (lets
        ($item (the item))
        ($first-char? (getter-item-first-char? (the item)))
        (getter-item
          $first-char?
          (eol?-list-getter
            (not? $first-char?)
            (getter-item-getter $item)))))
    ((non-empty-list item)
      (lets
        ($item (the item))
        (prepend $item (list item))))
    ((non-empty-separated separator item)
      (lets
        ($item (the item))
        (prepend
          $item
          (list (prefixed (the separator) $item)))))
    ((separated separator item)
      (or
        (optional (non-empty-separated separator item))
        null))
    ((or) (return #f))
    ((or x1) x1)
    ((or x1 x2)
      (lets
        ($x1 (the x1))
        ($x2 (the x2))
        (getter-item
          (or?
            (getter-item-first-char? $x1)
            (getter-item-first-char? $x2))
          (getter-switch (getter-item-getter $x1)
            ((false? _) (getter-item-getter $x2))
            ((%else $x1) (getter $x1))))))
    ((or x1 x2 xs ...)
      (or (or x1 x2) xs ...))
    ((string-append s ...)
      (apply (%string-append (the s) ...)))
    ((list->string l)
      (apply (%list->string l)))
    ((list-string l)
      (map l
        (lambda ($strings)
          (%apply %string-append $strings))))
    ((annotation item)
      (lets
        ($item item)
        (getter-item
          (getter-item-first-char? $item)
          (annotation-getter
            (getter-item-getter $item)
            stripped-annotation))))
    ((switch x ((pred? id) expr) ... ((else else-id) else-expr))
      (lets
        ($x (the x))
        (getter-item
          (getter-item-first-char? $x)
          (getter-switch (getter-item-getter $x)
            ((pred? id) (getter-item-getter (the expr)))
            ...
            ((%else else-id) (getter-item-getter (the else-expr))))))))

  (define digit (map (?char char-numeric?) %string string->number))

  (define-rule-syntax (check-parses parser in out)
    (check (datum/annotation=? (parse-string (the parser) in) out)))

  (define-rule-syntax (check-parse-error parser in)
    (check (raises (parse-string (the parser) in))))

  (define-rules-syntax (keywords ok error)
    ((check-parser-case parser (ok in out))
      (check-parses parser in out))
    ((check-parser-case parser (error in))
      (check-parse-error parser in)))

  (define-rule-syntax (check-parser parser case ...)
    (run (check-parser-case parser case) ...))
)
