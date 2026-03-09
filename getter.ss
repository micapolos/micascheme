(library (getter)
  (export
    getter-get!
    getter-sfd-get!
    getter-load!

    getter
    getter-bind
    getter-map
    getter-lets
    list->getter
    append-getter
    apply-getter
    error-getter

    sfd-getter
    bfp-getter
    line-number-getter
    column-number-getter

    indented-getter
    or-eof-getter

    skip-char-getter

    exact-char-getter
    exact-string-getter

    char/eof-getter
    char-getter

    peek-char/eof-getter
    peek-char-getter

    eof?-getter

    string-getter
    string-while-getter

    annotation-getter

    ; TODO: remove these, as they don't preserve line and column
    datum-annotation/eof-getter
    datum/eof-getter

    skip-until-getter
    skip-newlines-getter
    ending-getter

    newline-getter
    space-getter
    comma-getter
    colon-getter

    push-getter
    list-getter

    check-gets
    check-get-raises)
  (import
    (scheme)
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
    (throw)
    (char)
    (eof)
    (system)
    (source-file-descriptor))

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

  (define-rules-syntax
    ((getter ($port $sfd $indent $bfp $line $column) value/bfp/line/column)
      (lambda ($port $sfd $indent $bfp $line $column) value/bfp/line/column))
    ((getter $value)
      (getter ($port $sfd $indent $bfp $line $column)
        (values $value $bfp $line $column))))

  (define (getter-bind $getter $fn)
    (getter ($port $sfd $indent $bfp $line $column)
      (lets
        ((values $value $bfp $line $column)
          (getter-get! $getter $port $sfd $indent $bfp $line $column))
        (getter-get! ($fn $value) $port $sfd $indent $bfp $line $column))))

  (define-monadic getter)

  (define (raise-getter-error $message $datum $port $sfd $bfp)
    (raise
      (condition
        (make-message-condition $message)
        (make-i/o-read-error)
        (make-source-condition
          (make-annotation
            $datum
            (make-source-object $sfd $bfp $bfp)
            $datum)))))

  (define (error-getter $message $datum)
    (getter ($port $sfd $indent $bfp $line $column)
      (raise-getter-error $message $datum $port $sfd $bfp)))

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
      (lets
        ($char/eof (peek-char $port))
        (switch $char/eof
          ((eof? $eof)
            (cond
              ((zero? $column)
                (values $eof $bfp $line $column))
              ((<= $column $indent)
                (raise-getter-error "invalid" 'indent $eof $port $sfd $bfp))
              (else
                (values $eof $bfp $line $column))))
          ((char-newline? $newline)
            (cond
              ((zero? $column)
                (values (get-char $port) (+ $bfp 1) (+ $line 1) 0))
              ((<= $column $indent)
                (raise-getter-error "invalid" 'indent $port $sfd $bfp))
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
                    (raise-getter-error "invalid" 'indent $port $sfd $bfp))))
              (else
                (values (get-char $port) (+ $bfp 1) $line (+ $column 1)))))))))

  (define peek-char/eof-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (lets
        ($char/eof (peek-char $port))
        (switch $char/eof
          ((eof? $eof)
            (cond
              ((zero? $column)
                (values $eof $bfp $line $column))
              ((<= $column $indent)
                (raise-getter-error "invalid" 'indent $port $sfd $bfp))
              (else
                (values $eof $bfp $line $column))))
          ((char-newline? $newline)
            (cond
              ((zero? $column)
                (values $newline $bfp $line $column))
              ((<= $column $indent)
                (raise-getter-error "invalid" 'indent $port $sfd $bfp))
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
                    (raise-getter-error "invalid" 'indent $port $sfd $bfp))))
              (else
                (values $char $bfp $line $column))))))))

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
        ((eof? $eof) (throw char-getter $eof))
        ((else $char) (getter $char)))))

  (define peek-char-getter
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? $eof) (throw peek-char-getter $eof))
        ((else $char) (getter $char)))))

  (define (exact-char-getter $exact-char)
    (getter-lets
      ($char char-getter)
      (if (char=? $char $exact-char)
        (getter $char)
        (throw exact-char-getter $char))))

  (define (exact-string-getter $string)
    (getter-lets
      ($chars (list->getter (map exact-char-getter (string->list $string))))
      (getter (apply string $chars))))

  (define (skip-char-getter $getter)
    (getter-lets
      ($skipped-char char/eof-getter)
      $getter))

  (define (skip-until-getter $test? $getter)
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? $eof)
          (getter $eof))
        (($test? _)
          (skip-char-getter (skip-until-getter $test? $getter)))
        ((else $char)
          $getter))))

  (define (skip-newlines-getter $getter)
    (skip-until-getter char-newline? $getter))

  (define datum-annotation/eof-getter
    (getter ($port $sfd $indent $bfp $line $column)
      (lets
        ((values $datum/annotation $bfp)
          (get-datum/annotations $port $sfd $bfp))
        (values $datum/annotation $bfp $line $column))))

  (define datum/eof-getter
    (getter-lets
      ($datum-annotation/eof datum-annotation/eof-getter)
      (getter
        (switch $datum-annotation/eof
          ((eof? $eof) $eof)
          ((else $datum/annotation)
            (datum/annotation-stripped $datum/annotation))))))

  (define (push-while-getter $test? $chars)
    (getter-lets
      ($char/eof peek-char/eof-getter)
      (switch $char/eof
        ((eof? _)
          (getter $chars))
        (($test? $tested-char)
          (skip-char-getter
            (push-while-getter $test?
              (push $chars $tested-char))))
        ((else $untested-char)
          (getter $chars)))))

  (define (string-while-getter $test?)
    (getter-lets
      ($chars (push-while-getter $test? (stack)))
      (getter (apply string (reverse $chars)))))

  (define (push-chars-getter $chars)
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof? _) (getter $chars))
        ((else $char) (push-chars-getter (push $chars $char))))))

  (define string-getter
    (getter-lets
      ($chars (push-chars-getter (stack)))
      (getter (apply string (reverse $chars)))))

  (define (push-getter $stack $getter)
    (getter-lets
      ($value/eof $getter)
      (switch $value/eof
        ((eof? _)
          (getter $stack))
        ((else $value)
          (push-getter (push $stack $value) $getter)))))

  (define (list-getter $getter)
    (apply-getter reverse
      (push-getter (stack) $getter)))

  (define (annotation-getter $getter)
    (getter-lets
      ($bfp bfp-getter)
      ($value $getter)
      ($efp bfp-getter)
      ($sfd sfd-getter)
      (getter
        (make-annotation
          $value
          (make-source-object $sfd $bfp $efp)
          $value))))

  (define (ending-getter $getter $end-getter)
    (getter-lets
      ($value $getter)
      (_ $end-getter)
      (getter $value)))

  (define newline-getter (exact-char-getter #\newline))
  (define space-getter (exact-char-getter #\space))
  (define comma-getter (exact-char-getter #\,))
  (define colon-getter (exact-char-getter #\:))

  (define-rules-syntax
    ((check-gets getter string out)
      (check
        (equal?
          (lets
            ((values $value $bfp $line $column)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                0 0 0 0))
            $value)
          out)))
    ((check-gets getter string out bfp)
      (check
        (equal?
          (lets
            ((values $value $bfp $line $column)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                0 0 0 0))
            (list $value $bfp))
          (list out bfp))))
    ((check-gets getter string out bfp line column)
      (check
        (equal?
          (lets
            ((values $value $bfp $line $column)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                0 0 0 0))
            (list $value $bfp $line $column))
          (list out bfp line column)))))

  (define-rule-syntax (check-get-raises getter string)
    (check
      (raises
        (getter-get! getter
          (open-input-string string)
          (source-file-descriptor "test.txt" 0)
          0 0 0 0))))
)
