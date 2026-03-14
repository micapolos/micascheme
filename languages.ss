(library (languages)
  (export
    languages-extension-ref?
    languages-library-extensions
    languages-make-read
    languages-datum->language?-datum
    languages-expand

    languages-make-library-extensions
    languages-make-read
    languages-make-expand
    languages-call

    current-languages
    call-with-current-languages
    with-current-languages)
  (import
    (chezscheme)
    (syntax)
    (lets)
    (list)
    (switch)
    (boolean)
    (pair)
    (read)
    (eof)
    (procedure)
    (source-file-descriptor)
    (language))

  (define (languages-extension-ref? $languages $extension)
    (lets?
      ($tail (memp (partial extension-language=? $extension) $languages))
      (car $tail)))

  (define (languages-library-extensions $languages)
    (map language-library-extension $languages))

  (define (languages-make-read $languages $port $sfd $bfp $default-make-read)
    (lets
      ($extension (source-file-descriptor-extension $sfd))
      (switch (languages-extension-ref? $languages $extension)
        ((false? _)
          ($default-make-read $port $sfd $bfp))
        ((else $language)
          (language-make-read $language $port $sfd $bfp)))))

  (define (languages-datum->language? $languages $datum)
    (switch? $datum
      ((pair? $pair)
        (switch? (car $pair)
          ((string? $string)
            (languages-extension-ref? $languages $string))))))

  (define (languages-datum->language?-datum $languages $datum)
    (switch (languages-datum->language? $languages $datum)
      ((false? $false)
        (values $false $datum))
      ((language? $language)
        (values $language (cdr $datum)))))

  (define (languages-expand $languages $datum $environment)
    (switch (languages-datum->language? $languages $datum)
      ((false? _) $datum)
      ((else $language)
        (language-expand $language (cdr $datum) $environment))))

  ; === make procedures ===

  (define (languages-make-library-extensions $languages $default-library-extensions)
    (append
      (languages-library-extensions $languages)
      $default-library-extensions))

  (define (languages-make-read-handler $languages $make-read-handler)
    (lambda ($port $sfd $bfp)
      (languages-make-read $languages $port $sfd $bfp $make-read-handler)))

  (define (languages-make-expand $languages $default-expand)
    (lambda ($datum $environment . $args)
      (apply $default-expand
        (languages-expand $languages $datum $environment)
        $environment
        $args)))

  (define (languages-call $languages $procedure)
    (parameterize
      (
        (library-extensions
          (languages-make-library-extensions $languages (library-extensions)))
        (make-read-handler
          (languages-make-read-handler $languages (make-read-handler)))
        (current-expand
          (languages-make-expand $languages (current-expand))))
      ($procedure)))

  ; === current languages ===

  (define current-languages
    (make-thread-parameter null))

  (define (call-with-current-languages $procedure)
    (languages-call (current-languages) $procedure))

  (define-rule-syntax (with-current-languages x xs ...)
    (call-with-current-languages (lambda () x xs ...)))
)
