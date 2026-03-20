(library (mica reader)
  (export
    read-string
    read-port
    read-source-file-descriptor
    read-file

    return
    replace
    or
    error
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
    reject?-list
    skip-newlines
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
    check-reads
    check-read-error
    check-reader ok error
    annotation
    list-annotation
    cons-annotation
    lets
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
      (null %null)
      (lets %lets)
      (list-annotation %list-annotation)
      (cons-annotation %cons-annotation))
    (keyword)
    (getter))

  (define-keywords not > else ok)

  (define (read-string $reader $string)
    (read-port $reader (open-input-string $string) test-sfd 0))

  (define (read-port $reader $port $sfd $bfp)
    (%lets
      ((values $value $bfp $line $column)
        (getter-get!
          (ending-getter (getter-item-getter $reader) eof-getter)
          $port
          $sfd
          0 ; indent
          $bfp
          0
          0))
      $value))

  (define (read-source-file-descriptor $reader $sfd)
    (%lets
      ((values $value $bfp $line $column)
        (getter-get!
          (ending-getter (getter-item-getter $reader) eof-getter)
          (open-source-file $sfd)
          $sfd
          0 0 0 0))
      $value))

  (define (read-file $reader $path)
    (read-source-file-descriptor $reader
      (path->source-file-descriptor $path)))

  (define (error $message $datum)
    (getter-item
      (always #f)
      (error-getter $message $datum)))

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
        (keyword? id)
        #'(getter-item char? string-getter))))

  (define-syntax (char $syntax)
    (syntax-case $syntax ()
      ((_ ch)
        #'(getter-item
          (partial char=? (%char ch))
          (exact-char-getter (%char ch))))
      (id
        (keyword? id)
        #'(getter-item char? char-getter))))

  (define-syntax (null $syntax)
    (syntax-case $syntax ()
      (id
        (keyword? id)
        #'(return %null))))

  (define-rules-syntaxes (keywords else not >)
    ((return x)
      (getter-item
        (always #f)
        (getter x)))
    ((replace reader value)
      (%lets
        ($reader (the reader))
        (getter-item
          (getter-item-first-char? $reader)
          (replace-getter
            (getter-item-getter $reader)
            value))))
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
          (%lets
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
    ((first-char test? ... reader)
      (%lets
        ($reader (the reader))
        (getter-item
          (lambda ($char)
            (and
              (app (char-test test?) $char) ...
              ((getter-item-first-char? $reader) $char)))
          (getter-switch peek-char/eof-getter
            ((char? $char)
              (if (and (app (char-test test?) $char) ...)
                (getter-item-getter reader)
                (error-getter "unexpected char" $char)))
            ((%else $other)
              (getter-item-getter reader))))))
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
        (getter-item-first-char? (the $item))
        (optional-getter
          (getter-item-first-char? (the $item))
          (getter-item-getter (the $item)))))
    ((skip-newlines x)
      (%lets
        ($x (the x))
        (getter-item
          (or? (getter-item-first-char? $x) char-newline?)
          (skip-newlines-getter (getter-item-getter $x)))))
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
      (%lets
        ($item (the item))
        ($first-char? (getter-item-first-char? (the item)))
        (getter-item
          $first-char?
          (eol?-list-getter
            (not? $first-char?)
            (getter-item-getter $item)))))
    ((reject?-list reject? item)
      (%lets
        ($item (the item))
        (getter-item
          (or? reject? (getter-item-first-char? $item))
          (reject?-accept?-list-getter
            reject?
            (getter-item-first-char? $item)
            (getter-item-getter $item)))))
    ((non-empty-list item)
      (%lets
        ($item (the item))
        (prepend $item (list item))))
    ((non-empty-separated separator item)
      (%lets
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
      (%lets
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
      (%lets
        ($item (the item))
        (getter-item
          (getter-item-first-char? $item)
          (annotation-getter
            (getter-item-getter $item)
            stripped-annotation))))
    ((list-annotation list)
      (%lets
        ($list (the list))
        (getter-item
          (getter-item-first-char? $list)
          (annotation-getter
            (getter-item-getter $list)
            %list-annotation))))
    ((cons-annotation a b)
      (%lets
        ($a (the a))
        ($b (the b))
        (getter-item
          (getter-item-first-char? $a)
          (cons-annotation-getter
            (getter-item-getter $a)
            (getter-item-getter $b)))))
    ((lets (id expr) (ids exprs) ... body)
      (%lets
        ($expr (the expr))
        (getter-item
          (getter-item-first-char? $expr)
          (getter-lets
            (id (getter-item-getter $expr))
            (ids (getter-item-getter (the exprs))) ...
            (the (getter-item-getter body))))))
    ((switch x ((pred? id) expr) ... ((else else-id) else-expr))
      (%lets
        ($x (the x))
        (getter-item
          (getter-item-first-char? $x)
          (getter-switch (getter-item-getter $x)
            ((pred? id) (getter-item-getter (the expr)))
            ...
            ((%else else-id) (getter-item-getter (the else-expr))))))))

  (define digit (map (?char char-numeric?) %string string->number))

  (define-rule-syntax (check-reads reader in out)
    (check (datum/annotation=? (read-string (the reader) in) out)))

  (define-rule-syntax (check-read-error reader in)
    (check (raises (read-string (the reader) in))))

  (define-rules-syntax (keywords ok error)
    ((check-reader-case reader (ok in out))
      (check-reads reader in out))
    ((check-reader-case reader (error in))
      (check-read-error reader in)))

  (define-rule-syntax (check-reader reader case ...)
    (%lets
      ($reader (the reader))
      (run-void (check-reader-case $reader case) ...)))
)
