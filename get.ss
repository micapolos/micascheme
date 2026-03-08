(library (get)
  (export
    get
    getter
    getter-bind
    getter-lets
    list->getter
    append-getter
    apply-getter

    sfd-getter
    bfp-getter

    char-getter
    char-ungetter
    peek-char-getter

    eof?-getter
    char?-getter

    test?-string-getter

    datum/annotation-getter
    datum-getter

    annotation-getter

    check-gets)
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
    (stack))

  (define (get $getter $port $sfd $bfp)
    ($getter $port $sfd $bfp))

  (define (getter $value)
    (lambda ($port $sfd $bfp)
      (values $value $bfp)))

  (define (getter-bind $getter $fn)
    (lambda ($port $sfd $bfp)
      (lets
        ((values $value $bfp) (get $getter $port $sfd $bfp))
        (switch $value
          ((eof-object? $eof) (values $eof $bfp))
          ((else $other) (app ($fn $other) $port $sfd $bfp))))))

  (define sfd-getter
    (lambda ($port $sfd $bfp)
      (values $sfd $bfp)))

  (define bfp-getter
    (lambda ($port $sfd $bfp)
      (values $bfp $bfp)))

  (define eof?-getter
    (lambda ($port $sfd $bfp)
      (values (port-eof? $port) $bfp)))

  (define char?-getter
    (lambda ($port $sfd $bfp)
      (if (port-eof? $port)
        (values #f $bfp)
        (values (get-char $port) (+ $bfp 1)))))

  (define char-getter
    (lambda ($port $sfd $bfp)
      (values (get-char $port) (+ $bfp 1))))

  (define (char-ungetter $char)
    (lambda ($port $sfd $bfp)
      (lets
        (run (unget-char $port $char))
        (values $char (- $bfp 1)))))

  (define peek-char-getter
    (lambda ($port $sfd $bfp)
      (values (peek-char $port) $bfp)))

  (define-monadic getter)

  (define datum/annotation-getter get-datum/annotations)

  (define datum-getter
    (getter-lets
      ($datum/annotation datum/annotation-getter)
      (getter (datum/annotation-stripped $datum/annotation))))

  (define (test?-push-chars-getter $test? $chars)
    (getter-lets
      ($char? char?-getter)
      (switch $char?
        ((false? _)
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

  (define-rule-syntax (keywords values) (check-gets getter string out bfp)
    (lets
      ((values $value $bfp)
        (get getter
          (open-input-string string)
          (source-file-descriptor "test.txt" 0)
          0))
      (check
        (equal?
          `(,$value ,$bfp)
          `(,out ,bfp)))))
)
