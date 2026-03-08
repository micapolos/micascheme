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

    sfd-getter
    bfp-getter

    exact-char-getter
    exact-string-getter

    char/eof-getter
    char-getter
    char-ungetter

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
    (eof))

  (define (getter-get! $getter $port $sfd $indent $bfp $ip)
    ($getter $port $sfd $indent $bfp $ip))

  (define-rules-syntax
    ((getter ($port $sfd $indent $bfp $ip) value/bfp/ip)
      (lambda ($port $sfd $indent $bfp $ip) value/bfp/ip))
    ((getter $value)
      (getter ($port $sfd $indent $bfp $ip)
        (values $value $bfp $ip))))

  (define (getter-bind $getter $fn)
    (getter ($port $sfd $indent $bfp $ip)
      (lets
        ((values $value $bfp $ip)
          (getter-get! $getter $port $sfd $indent $bfp $ip))
        (getter-get! ($fn $value) $port $sfd $indent $bfp $ip))))

  (define sfd-getter
    (getter ($port $sfd $indent $bfp $ip)
      (values $sfd $bfp $ip)))

  (define bfp-getter
    (getter ($port $sfd $indent $bfp $ip)
      (values $bfp $bfp $ip)))

  (define eof?-getter
    (getter ($port $sfd $indent $bfp $ip)
      (values (port-eof? $port) $bfp $ip)))

  (define char/eof-getter
    (getter ($port $sfd $indent $bfp $ip)
      (lets
        ($char/eof (get-char $port))
        (values
          $char/eof
          (switch $char/eof
            ((eof? _) $bfp)
            ((else _) (+ $bfp 1)))
          $ip))))

  (define peek-char/eof-getter
    (getter ($port $sfd $indent $bfp $ip)
      (values (peek-char $port) $bfp $ip)))

  (define (char-ungetter $char)
    (getter ($port $sfd $indent $bfp $ip)
      (lets
        (run (unget-char $port $char))
        (values $char (- $bfp 1) $ip))))

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

  (define-monadic getter)

  (define datum-annotation/eof-getter
    (getter ($port $sfd $indent $bfp $ip)
      (lets
        ((values $datum/annotation $bfp)
          (get-datum/annotations $port $sfd $bfp))
        (values $datum/annotation $bfp $ip))))

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
        ((char-alphabetic? $char-alphabetic)
          (test?-push-chars-getter $test?
            (push $chars $char-alphabetic)))
        ((else $char-other)
          (getter-lets
            (_ (char-ungetter $char-other))
            (getter $chars))))))

  (define (test?-string-getter $test?)
    (getter-lets
      ($chars (test?-push-chars-getter $test? (stack)))
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
            ((values $value $bfp $ip)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                ""
                0
                0))
            $value)
          out)))
    ((check-gets getter string out bfp)
      (check
        (equal?
          (lets
            ((values $value $bfp $ip)
              (getter-get! getter
                (open-input-string string)
                (source-file-descriptor "test.txt" 0)
                ""
                0
                0))
            (list $value $bfp))
          (list out bfp))))
      ((check-gets getter string out bfp ip)
        (check
          (equal?
            (lets
              ((values $value $bfp $ip)
                (getter-get! getter
                  (open-input-string string)
                  (source-file-descriptor "test.txt" 0)
                  ""
                  0
                  0))
              (list $value $bfp))
            (list out bfp ip)))))

  (define-rule-syntax (check-get-raises getter string)
    (check
      (raises
        (getter-get! getter
          (open-input-string string)
          (source-file-descriptor "test.txt" 0)
          ""
          0
          0))))
)
