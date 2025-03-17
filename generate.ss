(library (generate)
  (export
    generate-symbol generate-symbols
    generate-temporary
    with-generate-temporary-seed
    with-tmps
    generates-identifier?
    generate-identifier)
  (import
    (scheme)
    (identifier)
    (number)
    (stack)
    (check)
    (lets)
    (syntax)
    (fluent))

  (define (generate-symbol)
    (syntax->datum (generate-identifier #'tmp)))

  (define (generate-symbols $count)
    (reverse
      (iterate
        (lambda ($stack)
          (push $stack (generate-symbol)))
        (stack)
        $count)))

  (define generate-temporary
    (case-lambda
      (()
        (or
          (generate-seeded-temporary)
          (car (generate-temporaries `(tmp)))))
      (($obj)
        (or
          (generate-seeded-temporary)
          (if (checking?)
            (build-identifier ($string $obj) (string-append "$" $string))
            (car (generate-temporaries (list $obj))))))))

  (define generate-temporary-seed-opt
    (make-thread-parameter #f))

  (define (generate-seeded-temporary)
    (lets
      ($seed-opt (generate-temporary-seed-opt))
      (and $seed-opt
        (let ()
          (generate-temporary-seed-opt (cons (car $seed-opt) (+ (cdr $seed-opt) 1)))
          (datum->syntax #`+
            (string->symbol
              (string-append
                (symbol->string (car $seed-opt))
                "-"
                (number->string (cdr $seed-opt)))))))))

  (define-rule-syntax (with-generate-temporary-seed $prefix $body ...)
    (parameterize ((generate-temporary-seed-opt (cons (quote $prefix) 0)))
      $body ...))

  (define-rule-syntax (with-tmps $body ...)
    (with-generate-temporary-seed $tmp $body ...))

  (define generates-identifier?
    (make-thread-parameter #t))

  (define (generate-identifier $id)
    (if (generates-identifier?)
      (parameterize
        ((gensym-prefix (string-append (symbol->string (syntax->datum $id)) "_")))
        (datum->syntax $id (string->symbol (symbol->string (syntax->datum (generate-temporary))))))
      $id))
)
