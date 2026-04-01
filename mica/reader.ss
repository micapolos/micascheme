(library (mica reader)
  (export
    read-port
    read-port-bfp
    read-string
    read-source-file-descriptor
    read-file

    return
    replace
    or
    error
    eof
    or-eof
    char ?char
    category-char
    range-char
    char>=
    char<=
    whitespace-char
    alphabetic-char
    numeric-char
    string
    logging
    lazy
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
    cons
    list
    list-of non-empty-list-of
    reject?-list-of
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
    (rename (micascheme)
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
      (list %list)
      (cons %cons)
      (list-annotation %list-annotation)
      (logging %logging)
      (flatten %flatten)
      (switch %switch)
      (null? %null?)
      (map %map)
      (always %always)
      (error %error)
      (eof %eof)
      (cons-annotation %cons-annotation))
    (keyword)
    (getter))

  (define-keywords not > else ok)

  (define (read-port $reader $port $sfd $bfp)
    (%lets
      ($getter (ending-getter (getter-item-getter $reader) eof-getter))
      ((values $value $bfp $line $column)
        (getter-get!
          $getter
          $port
          $sfd
          0 ; indent
          $bfp
          0
          0))
      $value))

  (define (read-port-bfp $reader $port $sfd $bfp)
    (%lets
      ($getter (getter-item-getter $reader))
      ((values $value $bfp $line $column)
        (getter-get!
          $getter
          $port
          $sfd
          0 ; indent
          $bfp
          0
          0))
      (values $value $bfp)))

  (define (read-string $reader $string)
    (read-port $reader (open-input-string $string) test-sfd 0))

  (define (read-source-file-descriptor $reader $sfd)
    (%lets
      ; Don't know why, but extracting this variable outside
      ; of lambda helped to avoid call-with-values warning
      ($port (open-source-file $sfd))
      ($getter (ending-getter (getter-item-getter $reader) eof-getter))
      ((values $value $bfp $line $column)
        (getter-get!
          $getter
          $port
          $sfd
          0
          0
          0
          0))
      $value))

  (define (read-file $reader $path)
    (read-source-file-descriptor $reader
      (path->source-file-descriptor $path)))

  (define error
    (case-lambda
      (($cause $datum $hint)
        (getter-item
          (%always #f)
          (error-getter $cause $datum $hint)))
      (($cause $datum)
        (getter-item
          (%always #f)
          (error-getter $cause $datum)))))

  (define eof (getter-item eof? eof-getter))

  (define (?char $test?)
    (getter-item
      (and? (not? eof?) $test?)
      (test?-char-getter $test?)))

  (define whitespace-char (?char char-whitespace?))
  (define alphabetic-char (?char char-alphabetic?))
  (define numeric-char (?char char-numeric?))

  (define alphabetic-string
    (getter-item
      (and? (not? eof?) char-alphabetic?)
      alphabetic-string-getter))

  (define numeric-string
    (getter-item
      (and? (not? eof?) char-numeric?)
      numeric-string-getter))

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
        (%always #t)
        (getter x)))
    ((replace reader value)
      (%lets
        ($reader (the reader))
        (getter-item
          (getter-item-first-char/eof? $reader)
          (replace-getter
            (getter-item-getter $reader)
            value))))
    ((the ch)
      (char? (datum ch))
      (getter-item
        (lambda ($char/eof)
          (and
            (char? $char/eof)
            (char=? $char/eof (datum ch))))
        (exact-char-getter (datum ch))))
    ((the s)
      (string? (datum s))
      (if (string-empty? (datum s))
        (getter-item
          eof?
          (exact-string-getter (datum s)))
        (getter-item
          (lambda ($char/eof)
            (and
              (char? $char/eof)
              (char=? $char/eof (string-ref (datum s) 0))))
          (exact-string-getter (datum s)))))
    ((the x) x)
    ((logging item)
      (lets
        ($item (the item))
        (return (%logging $item))))
    ((logging label item)
      (lets
        ($item (the item))
        (return (%logging label $item))))
    ((lazy item)
      (%lets
        ($lazy-item (lambda () (the item)))
        (getter-item
          (lambda ($char/eof)
            ((getter-item-first-char/eof? ($lazy-item)) $char/eof))
          (lazy-getter (lambda () (getter-item-getter ($lazy-item)))))))
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
          (lambda ($char/eof)
            (and
              (app (char-test test?) $char/eof) ...
              ((getter-item-first-char/eof? $reader) $char/eof)))
          (getter-switch peek-char/eof-getter
            ((char? $char)
              (if (and (app (char-test test?) $char) ...)
                (getter-item-getter reader)
                (error-getter '(unexpected char) $char)))
            ((%else $other)
              (getter-item-getter reader))))))
    ((prefixed $prefix $item)
      (getter-item
        (getter-item-first-char/eof? (the $prefix))
        (starting-getter
          (getter-item-getter (the $prefix))
          (getter-item-getter (the $item)))))
    ((suffixed $item $suffix)
      (getter-item
        (getter-item-first-char/eof? (the $item))
        (ending-getter
          (getter-item-getter (the $item))
          (getter-item-getter (the $suffix)))))
    ((wrapped $prefix $item $suffix)
      (suffixed (prefixed $prefix $item) $suffix))
    ((indented $item)
      (getter-item
        (getter-item-first-char/eof? (the $item))
        (indented-getter (getter-item-getter (the $item)))))
    ((optional $item)
      (getter-item
        (getter-item-first-char/eof? (the $item))
        (optional-getter
          (getter-item-first-char/eof? (the $item))
          (getter-item-getter (the $item)))))
    ((skip-newlines x)
      (%lets
        ($x (the x))
        (getter-item
          (or? (getter-item-first-char/eof? $x) char-newline?)
          (skip-newlines-getter (getter-item-getter $x)))))
    ((one-of x ...)
      (getter-item
        (or?
          (getter-item-first-char/eof? (the x))
          ...)
        (getter-switch peek-char/eof-getter
          (((getter-item-first-char/eof? (the x)) _)
            (getter-item-getter (the x)))
          ...
          ((%else $char/eof)
            (error-getter
              '(expected (one (of x ...)))
              $char/eof)))))
    ((or-eof item)
      (%lets
        ($item (the item))
        (getter-item
          (lambda ($char/eof)
            (or
              (eof? $char/eof)
              ((getter-item-first-char/eof? $item) $char/eof)))
          (or-eof-getter (getter-item-getter $item)))))
    ((map item fn)
      (getter-item
        (getter-item-first-char/eof? (the item))
        (getter-map (getter-item-getter (the item)) fn)))
    ((map item fn fns ...)
      (map (map item fn) fns ...))
    ((apply (fn item items ...))
      (getter-item
        (getter-item-first-char/eof? (the item))
        (apply-getter fn
          (getter-item-getter (the item))
          (getter-item-getter (the items)) ...)))
    ((prepend item list)
      (apply (%prepend (the item) (the list))))
    ((append item ...)
      (apply (%append (the item) ...)))
    ((cons car cdr)
      (lets
        ($car car)
        ($cdr cdr)
        (return (%cons $car $cdr))))
    ((list item ...)
      (apply (%list (the item) ...)))
    ((list-of item)
      (%lets
        ($item (the item))
        ($first-char/eof? (getter-item-first-char/eof? (the item)))
        (getter-item
          $first-char/eof?
          (eol?-list-getter
            (not? $first-char/eof?)
            (getter-item-getter $item)))))
    ((reject?-list-of reject? item)
      (%lets
        ($item (the item))
        (getter-item
          (or? reject? (getter-item-first-char/eof? $item))
          (reject?-accept?-list-getter
            reject?
            (getter-item-first-char/eof? $item)
            (getter-item-getter $item)))))
    ((non-empty-list-of item)
      (%lets
        ($item (the item))
        (prepend $item (list-of item))))
    ((non-empty-separated separator item)
      (%lets
        ($item (the item))
        (prepend
          $item
          (list-of (prefixed (the separator) $item)))))
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
            (getter-item-first-char/eof? $x1)
            (getter-item-first-char/eof? $x2))
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
          (getter-item-first-char/eof? $item)
          (annotation-getter
            (getter-item-getter $item)
            stripped-annotation))))
    ((list-annotation list)
      (%lets
        ($list (the list))
        (getter-item
          (getter-item-first-char/eof? $list)
          (annotation-getter
            (getter-item-getter $list)
            %list-annotation))))
    ((cons-annotation a b)
      (%lets
        ($a (the a))
        ($b (the b))
        (getter-item
          (getter-item-first-char/eof? $a)
          (cons-annotation-getter
            (getter-item-getter $a)
            (getter-item-getter $b)))))
    ((lets (id expr) (ids exprs) ... body)
      (%lets
        ($expr (the expr))
        (getter-item
          (getter-item-first-char/eof? $expr)
          (getter-lets
            (id (getter-item-getter $expr))
            (ids (getter-item-getter (the exprs))) ...
            (the (getter-item-getter body))))))
    ((switch x ((pred? id) expr) ... ((else else-id) else-expr))
      (%lets
        ($x (the x))
        (getter-item
          (getter-item-first-char/eof? $x)
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
