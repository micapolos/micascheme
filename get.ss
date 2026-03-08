(library (get)
  (export
    get
    getter
    getter-bind
    getter-lets
    list->getter
    append-getter
    apply-getter

    eof?-getter
    char?-getter
    char-getter
    char-ungetter

    datum/annotation-getter
    datum-getter

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
    (annotation))

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

  (define-monadic getter)

  (define datum/annotation-getter get-datum/annotations)

  (define datum-getter
    (getter-lets
      ($datum/annotation datum/annotation-getter)
      (getter (datum/annotation-stripped $datum/annotation))))

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
