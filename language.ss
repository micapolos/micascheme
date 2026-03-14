(library (language)
  (export
    language
    language?
    language-extensions
    language-make-read-procedure
    language-expand-procedure

    scheme-language
    extension-language=?

    language-library-extensions
    language-make-read
    language-expand

    list->language
    language-append

    language-call
    with-language
    define-language)
  (import
    (scheme)
    (lets)
    (switch)
    (eof)
    (data)
    (boolean)
    (procedure)
    (system)
    (throw)
    (source-file-descriptor)
    (syntax))

  (data (language extensions make-read-procedure expand-procedure))

  (define scheme-language
    (language
      (list "ss" "sls" "scm" "sch")
      default-make-read-handler
      sc-expand))

  (define (extension-language=? $extension $language)
    (not-false?
      (memp
        (partial string=? $extension)
        (language-extensions $language))))

  (define (language-make-read $language $port $sfd $bfp)
    ((language-make-read-procedure $language) $port $sfd $bfp))

  (define (language-expand $language $datum $environment)
    ((language-expand-procedure $language) $datum $environment))

  (define (language-library-extensions $language)
    (map library-extension (language-extensions $language)))

  (define (languages-extension-ref $languages $extension)
    (switch (memp (partial extension-language=? $extension) $languages)
      ((false? _)
        (throw languages-extension-ref $languages $extension))
      ((else $tail)
        (car $tail))))

  (define (list->language $languages)
    (language
      (apply append (map language-extensions $languages))
      (lambda ($port $sfd $bfp)
        (lets
          ($extension (source-file-descriptor-extension $sfd))
          ($language (languages-extension-ref $languages $extension))
          ($read (language-make-read $language $port $sfd $bfp))
          (lambda ()
            (switch ($read)
              ((eof? $eof) $eof)
              ((else $value)
                (cons
                  (language-expand-procedure $language)
                  $value))))))
      (lambda ($datum $environment . $args)
        (or
          (switch? $datum
            ((pair? $pair)
              (switch? (car $pair)
                ((procedure? $procedure)
                  (apply $procedure (cdr $datum) $environment $args)))))
          (throw invalid-languages-datum $languages $datum)))))

  (define (language-append . $languages)
    (list->language $languages))

  (define (language-call $language $procedure)
    (parameterize
      (
        (library-extensions (language-library-extensions $language))
        (make-read-handler (language-make-read-procedure $language))
        (current-expand (language-expand-procedure $language)))
      ($procedure)))

  (define-rule-syntax (with-language language x xs ...)
    (language-call language
      (lambda () x xs ...)))

  (define-rule-syntax (define-language id language)
    (define-rule-syntax (id x xs (... ...))
      (with-language language x xs (... ...))))
)
