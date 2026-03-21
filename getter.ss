(library (getter)
  (export
    getter-get!
    getter-sfd-get!
    getter-load!

    getter
    getter-bind
    getter-map
    getter-lets
    getter-switch
    list->getter
    append-getter
    replace-getter
    apply-getter
    error-getter

    sfd-getter
    bfp-getter
    line-number-getter
    column-number-getter

    indented-getter
    or-eof-getter

    skip-char-getter

    exact-getter
    exact-char-getter
    exact-string-getter

    char/eof-getter
    char-getter
    test?-char-getter
    eof-getter

    peek-char/eof-getter
    peek-char-getter

    eof?-getter

    string-getter
    string-while-getter

    annotation-getter
    cons-annotation-getter

    skip-until-getter
    skip-newlines-getter

    starting-getter
    enclosing-getter
    ending-getter

    separated-getter
    non-empty-separated-getter

    newline-getter
    space-getter
    comma-getter
    colon-getter

    optional-getter

    eol?-push-getter
    push-getter

    eol?-list-getter
    list-getter

    reject?-accept?-push-getter
    reject?-accept?-list-getter

    alphabetic-string-getter
    numeric-string-getter

    test-sfd
    test-source-object
    check-gets
    check-get-raises

    getter-item
    getter-item?
    getter-item-getter
    getter-item-first-char?)
  (import
    (scheme)
    (data)
    (lets)
    (monadic)
    (procedure)
    (list)
    (syntax)
    (syntaxes)
    (check)
    (switch)
    (annotation)
    (boolean)
    (stack)
    (char)
    (eof)
    (system)
    (source-file-descriptor)
    (condition)
    (predicate))

  (define indent-size 2)

  (define (getter-get! $getter $port $sfd $indent $bfp $line $column)
    ($getter $port $sfd $indent $bfp $line $column))

  (define (getter-sfd-get! $getter $sfd)
    (lets
      ((values $value $bfp $line $column)
        (getter-get! $getter
          (open-source-file $sfd)
          $sfd
          0 0 0 0))
      $value))

  (define (getter-load! $getter $path)
    (getter-sfd-get! $getter (path->source-file-descriptor $path)))

  (define-rules-syntaxes
    ((getter ($port $sfd $indent $bfp $line $column) value/bfp/line/column)
      (lambda ($port $sfd $indent $bfp $line $column) value/bfp/line/column))
    ((getter $value)
      (getter ($port $sfd $indent $bfp $line $column)
        (values $value $bfp $line $column)))
    ((exact-getter ch)
      (char? (datum ch))
      (exact-char-getter ch))
    ((exact-getter s)
      (string? (datum s))
      (exact-string-getter s)))

  (define (getter-bind $getter $fn)
    (getter ($port $sfd $indent $bfp $line $column)
      (lets
        ((values $value $bfp $line $column)
          (getter-get! $getter $port $sfd $indent $bfp $line $column))
        (getter-get! ($fn $value) $port $sfd $indent $bfp $line $column))))

  (define-monadic getter)

  (define raise-getter-error
    (case-lambda
      (($cause $datum $port $sfd $bfp $hint)
        (raise
          (condition
            (make-i/o-read-error)
            (make-source-condition
              (make-annotation
                $datum
                (make-source-object $sfd $bfp $bfp)
                $datum))
            (make-cause-condition $cause)
            (make-hint-condition $hint))))
      (($cause $datum $port $sfd $bfp)
        (raise
          (condition
            (make-i/o-read-error)
            (make-source-condition
              (make-annotation
                $datum
                (make-source-object $sfd $bfp $bfp)
                $datum))
            (make-cause-condition $cause))))))

  (define error-getter
    (case-lambda
      (($cause $datum $hint)
        (getter ($port $sfd $indent $bfp $line $column)
          (raise-getter-error $cause $datum $port $sfd $bfp $hint)))
      (($cause $datum)
        (getter ($port $sfd $indent $bfp $line $column)
          (raise-getter-error $cause $datum $port $sfd $bfp)))))

  (define sfd-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (values $sfd $bfp $line $column)))

  (define bfp-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (values $bfp $bfp $line $column)))

  (define line-number-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (values (+ $line 1) $bfp $line $column)))

  (define column-number-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (values (+ $column 1) $bfp $line $column)))

  (define eof?-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (values (port-eof? $port) $bfp $line $column)))

  (define (indented-getter $getter)
    (getter ($port $sfd $indent $bfp $line $column)
      (getter-get! $getter $port $sfd (+ $indent indent-size) $bfp $line $column)))

  (define char/eof-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (switch (peek-char $port)
        ((eof? $eof)
          (cond
            ((zero? $column)
              (values $eof $bfp $line $column))
            ((<= $column $indent)
              (raise-getter-error
                '(eof (inside indent))
                $eof $port $sfd $bfp))
            (else
              (values $eof $bfp $line $column))))
        ((char-newline? $newline)
          (cond
            ((zero? $column)
              (values (get-char $port) (+ $bfp 1) (+ $line 1) 0))
            ((<= $column $indent)
              (raise-getter-error
                '(newline (after indent))
                $newline $port $sfd $bfp))
            (else
              (values (get-char $port) (+ $bfp 1) (+ $line 1) 0))))
        ((else $char)
          (cond
            ((< $column $indent)
              (cond
                ((char=? $char #\space)
                  (getter-get!
                    (run (get-char $port) char/eof-getter)
                    $port $sfd $indent (+ $bfp 1) $line (+ $column 1)))
                ((zero? (mod $column indent-size))
                  (values eof $bfp $line $column))
                (else
                  (raise-getter-error
                    '(invalid indent)
                    $char $port $sfd $bfp
                    '(indent (should (contain (exactly (two spaces)))))))))
            (else
              (values (get-char $port) (+ $bfp 1) $line (+ $column 1))))))))

  (define peek-char/eof-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (switch (peek-char $port)
        ((eof? $eof)
          (cond
            ((zero? $column)
              (values $eof $bfp $line $column))
            ((<= $column $indent)
              (raise-getter-error
                '(eof (inside indent))
                $eof $port $sfd $bfp))
            (else
              (values $eof $bfp $line $column))))
        ((char-newline? $newline)
          (cond
            ((zero? $column)
              (values $newline $bfp $line $column))
            ((<= $column $indent)
              (raise-getter-error
                '(newline (after indent))
                $newline $port $sfd $bfp))
            (else
              (values $newline $bfp $line $column))))
        ((else $char)
          (cond
            ((< $column $indent)
              (cond
                ((char=? $char #\space)
                  (getter-get!
                    (run (get-char $port) peek-char/eof-getter)
                    $port $sfd $indent (+ $bfp 1) $line (+ $column 1)))
                ((zero? (mod $column indent-size))
                  (values eof $bfp $line $column))
                (else
                  (raise-getter-error
                    '(invalid indent)
                    $char $port $sfd $bfp
                    '(indent (should (contain (exactly (two spaces)))))))))
            (else
              (values $char $bfp $line $column)))))))

  (define eof-getter
    (getter-switch peek-char/eof-getter
      ((eof? $eof) (getter $eof))
      ((else $char) (error-getter '(expected eof) $char))))

  (define (or-eof-getter $getter)
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? $eof) (getter $eof))
        ((else _) $getter))))

  (define char-getter
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof? $eof) (error-getter '(unexpected eof) $eof))
        ((else $char) (getter $char)))))

  (define peek-char-getter
    (getter-switch peek-char/eof-getter
      ((eof? $eof) (error-getter '(unexpected eof) $eof))
      ((else $char) (getter $char))))

  (define (exact-char-getter $exact-char)
    (getter-lets
      ($char char-getter)
      (if (char=? $char $exact-char)
        (getter $char)
        (error-getter
          '(unexpected char)
          $char
          `(expected ,$exact-char)))))

  (define (exact-string-getter $string)
    (getter-lets
      ($chars (list->getter (map exact-char-getter (string->list $string))))
      (getter (apply string $chars))))

  (define (test?-char-getter $test?)
    (getter-lets
      ($char char-getter)
      (switch $char
        (($test? $char) (getter $char))
        ((else $char) (error-getter '(unexpected char) $char)))))

  (define (skip-char-getter $getter)
    (getter-lets
      ($skipped-char char/eof-getter)
      $getter))

  (define (skip-until-getter $test? $getter)
    (getter-switch peek-char/eof-getter
      ((eof? $eof) $getter)
      (($test? _)
        (skip-char-getter (skip-until-getter $test? $getter)))
      ((else $char)
        $getter)))

  (define (skip-newlines-getter $getter)
    (skip-until-getter char-newline? $getter))

  (define (push-while-getter $test? $chars)
    (getter-switch peek-char/eof-getter
      ((eof? _)
        (getter $chars))
      (($test? $tested-char)
        (skip-char-getter
          (push-while-getter $test?
            (push $chars $tested-char))))
      ((else $untested-char)
        (getter $chars))))

  (define (string-while-getter $test?)
    (getter-lets
      ($chars (push-while-getter $test? (stack)))
      (getter (apply string (reverse $chars)))))

  (define (push-chars-getter $chars)
    (getter-switch char/eof-getter
      ((eof? _) (getter $chars))
      ((else $char) (push-chars-getter (push $chars $char)))))

  (define string-getter
    (getter-lets
      ($chars (push-chars-getter (stack)))
      (getter (apply string (reverse $chars)))))

  (define alphabetic-string-getter
    (string-while-getter char-alphabetic?))

  (define numeric-string-getter
    (string-while-getter char-numeric?))

  (define (reject?-accept?-push-getter $reject? $accept? $stack $getter)
    (getter-switch peek-char/eof-getter
      ((eof? _)
        (getter $stack))
      (($reject? _)
        (getter-lets
          (_ char-getter)
          (reject?-accept?-push-getter
            $reject?
            $accept?
            $stack
            $getter)))
      (($accept? _)
        (getter-lets
          ($value $getter)
          (reject?-accept?-push-getter
            $reject?
            $accept?
            (push $stack $value)
            $getter)))
      ((else _)
        (getter $stack))))

  (define (reject?-accept?-list-getter $reject? $accept? $getter)
    (apply-getter reverse
      (reject?-accept?-push-getter $reject? $accept? (stack) $getter)))

  (define (eol?-push-getter $eol? $stack $getter)
    (getter-switch peek-char/eof-getter
      ((eof? _)
        (getter $stack))
      (($eol? _)
        (getter $stack))
      ((else _)
        (getter-switch $getter
          ((eof? _)
            (getter $stack))
          ((else $value)
            (eol?-push-getter
              $eol?
              (push $stack $value)
              $getter))))))

  (define (push-getter $stack $getter)
    (eol?-push-getter (lambda (_) #f) $stack $getter))

  (define (eol?-list-getter $eol? $getter)
    (apply-getter reverse
      (eol?-push-getter $eol? (stack) $getter)))

  (define (list-getter $getter)
    (apply-getter reverse
      (push-getter (stack) $getter)))

  (define (annotation-getter $getter $make-annotation)
    (getter-lets
      ($bfp bfp-getter)
      ($value $getter)
      ($efp bfp-getter)
      ($sfd sfd-getter)
      (getter
        ($make-annotation $value
          (make-source-object $sfd $bfp $efp)))))

  (define (cons-annotation-getter $car-getter $cdr-getter)
    (getter-lets
      ($bfp bfp-getter)
      ($car $car-getter)
      ($cdr $cdr-getter)
      ($efp bfp-getter)
      ($sfd sfd-getter)
      (getter
        (cons-annotation $car $cdr
          (make-source-object $sfd $bfp $efp)))))

  (define (starting-getter $start-getter $getter)
    (getter-lets
      (_ $start-getter)
      ($value $getter)
      (getter $value)))

  (define (enclosing-getter $start-getter $getter $end-getter)
    (getter-lets
      (_ $start-getter)
      ($value $getter)
      (_ $end-getter)
      (getter $value)))

  (define (ending-getter $getter $end-getter)
    (getter-lets
      ($value $getter)
      (_ $end-getter)
      (getter $value)))

  (define newline-getter (exact-getter #\newline))
  (define space-getter (exact-getter #\space))
  (define comma-getter (exact-getter #\,))
  (define colon-getter (exact-getter #\:))

  (define (optional-getter $first-char? $getter)
    (getter-switch peek-char/eof-getter
      ((eof? _) (getter #f))
      (($first-char? $first-char) $getter)
      ((else _) (getter #f))))

  ; predicate for first char and the getter itself.
  (data (getter-item first-char? getter))

  (define (optional-item-getter $item)
    (optional-getter
      (getter-item-first-char? $item)
      (getter-item-getter $item)))

  (define (separated-getter $getter-item $separator-getter-item)
    (getter-switch (optional-item-getter $getter-item)
      ((false? _) (getter null))
      ((else $item)
        (getter-switch (optional-item-getter $separator-getter-item)
          ((false? _) (getter (list $item)))
          ((else _)
            (apply-getter cons
              (getter $item)
              (non-empty-separated-getter
                (getter-item-getter $getter-item)
                $separator-getter-item)))))))

  (define (non-empty-separated-getter $item-getter $separator-getter-item)
    (apply-getter cons
      $item-getter
      (eol?-list-getter
        (not? (getter-item-first-char? $separator-getter-item))
        (starting-getter
          (getter-item-getter $separator-getter-item)
          $item-getter))))

  (define test-sfd (source-file-descriptor "test.txt" 0))

  (define (test-source-object $bfp $efp)
    (make-source-object test-sfd $bfp $efp))

  (define-rules-syntaxes
    ((check-gets getter string out)
      (check
        (datum/annotation=?
          (lets
            ((values $value $bfp $line $column)
              (getter-get! getter
                (open-input-string string)
                test-sfd
                0 0 0 0))
            $value)
          out)))
    ((check-gets getter string out bfp)
      (check
        (datum/annotation=?
          (lets
            ((values $value $bfp $line $column)
              (getter-get! getter
                (open-input-string string)
                test-sfd
                0 0 0 0))
            (list $value $bfp))
          (list out bfp))))
    ((check-gets getter string out bfp line column)
      (check
        (datum/annotation=?
          (lets
            ((values $value $bfp $line $column)
              (getter-get! getter
                (open-input-string string)
                test-sfd
                0 0 0 0))
            (list $value $bfp $line $column))
          (list out bfp line column))))
    ((check-get-raises getter string)
      (check
        (raises
          (getter-get! getter
            (open-input-string string)
            test-sfd
            0 0 0 0)))))
)
