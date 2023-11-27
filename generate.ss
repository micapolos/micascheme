(library (generate)
  (export
    generate-symbol generate-symbols
    generate-temporary
    with-generate-temporary-seed
    with-tmps)
  (import
    (scheme)
    (identifier)
    (iterate)
    (stack)
    (check)
    (lets)
    (syntax))

  (define (generate-symbol)
    (syntax->datum (generate-temporary)))

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

  (define-syntax-rule (with-generate-temporary-seed $prefix $body ...)
    (parameterize ((generate-temporary-seed-opt (cons (quote $prefix) 0)))
      $body ...))

  (define-syntax-rule (with-tmps $body ...)
    (with-generate-temporary-seed $tmp $body ...))


)
