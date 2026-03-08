(library (get)
  (export
    get
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
    (check)
    (switch)
    (annotation)
    (boolean)
    (stack)
    (throw))

  (define (get $getter $port $sfd $bfp)
    ($getter $port $sfd $bfp))

  (define (getter $value)
    (lambda ($port $sfd $bfp)
      (values $value $bfp)))

  (define (getter-bind $getter $fn)
    (lambda ($port $sfd $bfp)
      (lets
        ((values $value $bfp) (get $getter $port $sfd $bfp))
        (app ($fn $value) $port $sfd $bfp))))

  (define sfd-getter
    (lambda ($port $sfd $bfp)
      (values $sfd $bfp)))

  (define bfp-getter
    (lambda ($port $sfd $bfp)
      (values $bfp $bfp)))

  (define eof?-getter
    (lambda ($port $sfd $bfp)
      (values (port-eof? $port) $bfp)))

  (define char/eof-getter
    (lambda ($port $sfd $bfp)
      (lets
        ($char/eof (get-char $port))
        (values
          $char/eof
          (switch $char/eof
            ((eof-object? _) $bfp)
            ((else _) (+ $bfp 1)))))))

  (define peek-char/eof-getter
    (lambda ($port $sfd $bfp)
      (values (peek-char $port) $bfp)))

  (define char-getter
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof-object? $eof) (throw char-getter $eof))
        ((else $char) (getter $char)))))

  (define (char-ungetter $char)
    (lambda ($port $sfd $bfp)
      (lets
        (run (unget-char $port $char))
        (values $char (- $bfp 1)))))

  (define (exact-char-getter $exact-char)
    (getter-lets
      ($char char-getter)
      (if (char=? $char $exact-char)
        (getter $char)
        (throw exact-char-getter $char))))

  (define (skip-until-getter $test?)
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof-object? $eof-object) (getter $eof-object))
        (($test? _) (skip-until-getter $test?))
        ((else $char) (char-ungetter $char)))))

  (define-monadic getter)

  (define datum-annotation/eof-getter get-datum/annotations)

  (define datum/eof-getter
    (getter-lets
      ($datum-annotation/eof datum-annotation/eof-getter)
      (getter
        (switch $datum-annotation/eof
          ((eof-object? $eof-object) $eof-object)
          ((else $datum/annotation) (datum/annotation-stripped $datum/annotation))))))

  (define (test?-push-chars-getter $test? $chars)
    (getter-lets
      ($char/eof char/eof-getter)
      (switch $char/eof
        ((eof-object? _)
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
        ((eof-object? _)
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

  (define-rule-syntax (check-gets getter string out bfp)
    (check
      (equal?
        (values->list
          (get getter
            (open-input-string string)
            (source-file-descriptor "test.txt" 0)
            0))
        `(,out ,bfp))))

  (define-rule-syntax (check-get-raises getter string)
    (check
      (raises
        (get getter
          (open-input-string string)
          (source-file-descriptor "test.txt" 0)
          0))))
)
