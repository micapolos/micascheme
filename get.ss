(library (get)
  (export
    get
    getter
    getter-bind
    getter-lets
    list->getter
    append-getter
    apply-getter

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
    (switch))

  (define (get $getter $port $sfd $bfp)
    ($getter $port $sfd $bfp))

  (define (getter $value)
    (lambda ($port $sfd $bfp)
      (values $bfp $value)))

  (define (getter-bind $getter $fn)
    (lambda ($port $sfd $bfp)
      (lets
        ((values $value $bfp) (get $getter $port $sfd $bfp))
        (switch $value
          ((eof-object? $eof) (values $eof $bfp))
          ((else $other) (app ($fn $other) $port $sfd $bfp))))))

  (define-monadic getter)

  (define (datum-getter $port $sfd $bfp)
    (get-datum/annotations $port $sfd $bfp))

  (define-rule-syntax (keywords values) (check-gets getter string (values out bfp))
    (lets
      ((values $annotation $bfp)
        (get getter
          (open-input-string string)
          (source-file-descriptor "test.txt" 0)
          0))
      (check
        (equal?
          `(values ,(annotation-stripped $annotation) ,$bfp)
          `(values out ,bfp)))))
)
