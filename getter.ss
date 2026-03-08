(library (getter)
  (export
    getter-get!

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

    exact-char-getter
    exact-string-getter

    char/eof-getter
    string-getter
    char-getter
    char-ungetter
    indented-getter

    peek-char/eof-getter

    eof?-getter

    test?-string-getter

    datum-annotation/eof-getter
    datum/eof-getter

    annotation-getter
    skip-until-getter
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
    (system))

  (define indent-size 2)

  (define (getter-get! $getter $port $sfd $indent $bfp $column)
    ($getter $port $sfd $indent $bfp $column))

  (define-rules-syntax
    ((getter ($port $sfd $indent $bfp $column) value/bfp/ip)
      (lambda ($port $sfd $indent $bfp $column) value/bfp/ip))
    ((getter $value)
      (getter ($port $sfd $indent $bfp $column)
        (values $value $bfp $column))))

  (define (getter-bind $getter $fn)
    (getter ($port $sfd $indent $bfp $column)
      (lets
        ((values $value $bfp $column)
          (getter-get! $getter $port $sfd $indent $bfp $column))
        (getter-get! ($fn $value) $port $sfd $indent $bfp $column))))

  (define-monadic getter)

  (define (raise-getter-error $message $port $sfd $bfp $efp)
    (raise
      (condition
        (make-message-condition $message)
        (make-i/o-read-error)
        (make-source-condition (make-source-object $sfd $bfp $efp)))))

  (define (error-getter $message $bfp $efp)
    (getter ($port $sfd $indent $bfp $column)
      (raise-getter-error $message $port $sfd $bfp $efp)))

  (define sfd-getter
    (getter ($port $sfd $indent $bfp $column)
      (values $sfd $bfp $column)))

  (define bfp-getter
    (getter ($port $sfd $indent $bfp $column)
      (values $bfp $bfp $column)))

  (define eof?-getter
    (getter ($port $sfd $indent $bfp $column)
      (values (port-eof? $port) $bfp $column)))

  (define char/eof-getter
    (getter ($port $sfd $indent $bfp $column)
      (lets
        ($char/eof (peek-char $port))
        (switch $char/eof
          ((eof? $eof)
            (cond
              ((zero? $column)
                (values $eof $bfp $column))
              ((<= $column $indent)
                (raise-getter-error "eof during indent" $port $sfd $bfp (+ $bfp 1)))
              (else
                (values $eof $bfp $column))))
          ((char-newline? $newline)
            (cond
              ((zero? $column)
                (values (get-char $port) (+ $bfp 1) 0))
              ((<= $column $indent)
                (raise-getter-error "empty indent" $port $sfd $bfp (+ $bfp 1)))
              (else
                (values (get-char $port) (+ $bfp 1) 0))))
          ((else $char)
            (cond
              ((< $column $indent)
                (cond
                  ((char=? $char #\space)
                    (lets
                      (run (get-char $port))
                      (char/eof-getter $port $sfd $indent (+ $bfp 1) (+ $column 1))))
                  ((zero? (mod $column indent-size))
                    (values eof $bfp $column))
                  (else
                    (raise-getter-error "invalid indent char" $port $sfd $bfp (+ $bfp 1)))))
              (else
                (values (get-char $port) (+ $bfp 1) (+ $column 1)))))))))

  (define (indented-getter $getter)
    (getter ($port $sfd $indent $bfp $column)
      (getter-get! $getter $port $sfd (+ $indent 2) $bfp $column)))

  (define peek-char/eof-getter
    (getter ($port $sfd $indent $bfp $column)
      (values (peek-char $port) $bfp $column)))

  (define (char-ungetter $char)
    (getter ($port $sfd $indent $bfp $column)
      (lets
        (run (unget-char $port $char))
        (values $char (- $bfp 1) $column))))

  (define char-getter
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof? $eof) (throw char-getter $eof))
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

  (define (skip-until-getter $test?)
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof? $eof) (getter $eof))
        (($test? _) (skip-until-getter $test?))
        ((else $char) (char-ungetter $char)))))

  (define datum-annotation/eof-getter
    (getter ($port $sfd $indent $bfp $column)
      (lets
        ((values $datum/annotation $bfp)
          (get-datum/annotations $port $sfd $bfp))
        (values $datum/annotation $bfp $column))))

  (define datum/eof-getter
    (getter-lets
      ($datum-annotation/eof datum-annotation/eof-getter)
      (getter
        (switch $datum-annotation/eof
          ((eof? $eof) $eof)
          ((else $datum/annotation)
            (datum/annotation-stripped $datum/annotation))))))

  (define (test?-push-chars-getter $test? $chars)
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof? _)
          (getter $chars))
        (($test? $tested-char)
          (test?-push-chars-getter $test?
            (push $chars $tested-char)))
        ((else $untested-char)
          (getter-lets
            (_ (char-ungetter $untested-char))
            (getter $chars))))))

  (define (test?-string-getter $test?)
    (getter-lets
      ($chars (test?-push-chars-getter $test? (stack)))
      (getter (apply string (reverse $chars)))))

  (define string-getter
    (test?-string-getter (lambda (_) #t)))

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
            ((values $value $bfp $column)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                0
                0
                0))
            $value)
          out)))
    ((check-gets getter string out bfp)
      (check
        (equal?
          (lets
            ((values $value $bfp $column)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                0
                0
                0))
            (list $value $bfp))
          (list out bfp))))
      ((check-gets getter string out bfp ip)
        (check
          (equal?
            (lets
              ((values $value $bfp $column)
                (getter-get! getter
                  (open-input-string string)
                  (source-file-descriptor "test.txt" 0)
                  0
                  0
                  0))
              (list $value $bfp $column))
            (list out bfp ip)))))

  (define-rule-syntax (check-get-raises getter string)
    (check
      (raises
        (getter-get! getter
          (open-input-string string)
          (source-file-descriptor "test.txt" 0)
          0
          0
          0))))
)
